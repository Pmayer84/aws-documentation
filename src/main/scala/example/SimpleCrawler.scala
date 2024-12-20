package io.scalastic.aws

import io.scalastic.aws.PeriodicSave.{Content, savePeriodically}
import io.scalastic.aws.Model
import scala.util.{Failure, Success}
import sttp.model.Uri
import java.io.{File, IOException}
import java.net.URLDecoder
import scala.xml.XML
import sttp.client3._
import ujson._
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import scala.collection.mutable
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object SimpleCrawler extends AwsWebScraper with IO with Model with App {

  val rootDocumentationUrl: String = "https://docs.aws.amazon.com"
  val entrypointDocumentationUrl: String = rootDocumentationUrl
  val documentationPath: String = "./data/"
  val outputFilename: String = "SimpleCrawler.json"
  val maxDepth = 5 // Set maximum depth for crawling
  val maxConcurrentCrawls = 10 // Set maximum concurrent crawls

  var pageCount = 0 // Global counter for the number of pages crawled
  val visitedUrls = mutable.Set[String]() // Set to track visited URLs

  // Create the directory if it doesn't exist
  val directory = new File(documentationPath)
  if (!directory.exists()) {
    println(s"Creating directory: $documentationPath")
    directory.mkdirs()
  } else {
    println(s"Directory already exists: $documentationPath")
  }

  def normalizeUrl(url: String): String = {
  // Remove trailing dots before adding the root URL if necessary
  val cleanedUrl = url.stripSuffix(".") // Remove any trailing dot
  
  val normalizedUrl = if (cleanedUrl.startsWith("http")) cleanedUrl else rootDocumentationUrl + cleanedUrl
  normalizedUrl
}


  // Function to check if the page contains a PGP public key block
  def containsPgppublicKey(doc: Document): Boolean = {
    val pgpPattern = """-----BEGIN PGP PUBLIC KEY BLOCK-----[\s\S]*?-----END PGP PUBLIC KEY BLOCK-----""".r
    val bodyText = doc.select("body").text() // Get all text from the <body> tag
    pgpPattern.findFirstIn(bodyText).isDefined // Check if the PGP key block pattern is found
  }

  def crawlAndCategorize(url: String, depth: Int): Future[Seq[Content]] = {
    if (depth > maxDepth || visitedUrls.contains(url) || url.endsWith(".pdf") || url.contains(".rss") || url.contains("#")) {
      return Future.successful(Seq()) // Skip fragment identifiers
    }

    val resolvedUrl = normalizeUrl(url)
    if (!resolvedUrl.contains("docs.aws.amazon.com")) {
      println(s"Skipping non-AWS documentation URL: $resolvedUrl")
      return Future.successful(Seq()) // Skip non-AWS documentation URLs
    }

    visitedUrls += resolvedUrl
    pageCount += 1
    println(s"Page Count: $pageCount") // Log the page count

    try {
      val htmlContent = fetchHtmlFromUrl(resolvedUrl)
      if (htmlContent.isEmpty) return Future.successful(Seq()) // Return an empty Future with a sequence

      val doc = Jsoup.parse(htmlContent)

      // Skip pages without body content or misleading titles
      if (doc == null || doc.select("body").isEmpty || doc.title().equalsIgnoreCase("PDF Document")) {
        println(s"Skipping page without body content or PDF document: $resolvedUrl")
        return Future.successful(Seq()) // Return an empty Future with a sequence
      }

      // Detect and skip pages with embedded PDF content
      val embedTags = doc.select("embed[type=application/x-google-chrome-pdf]")
      if (!embedTags.isEmpty) {
        println(s"Skipping page with embedded PDF content: $resolvedUrl")
        return Future.successful(Seq()) // Return an empty Future with a sequence
      }

      // Check for meta tags indicating the page should be skipped
      val isDecisionGuide = doc.select("meta[name=guide][content=AWS Decision Guide]").size() > 0
      val isLandingPage = doc.select("meta[name=guide-name][content=Landing Page]").size() > 0

      // Check for PGP public key block in the page
      if (containsPgppublicKey(doc)) {
        println(s"Skipping page with PGP public key block: $resolvedUrl")
        return Future.successful(Seq()) // Return an empty Future with a sequence
      }

      // Extract links for further crawling
      val links = findLinksInPage(htmlContent).map(normalizeUrl).filter(_.nonEmpty)

      if (isDecisionGuide || isLandingPage) {
        println(s"Skipping scraping content due to meta tag: $resolvedUrl")
        // Continue to crawl the links found on this page
        val batchedFutures: Seq[Future[Seq[Content]]] = links.grouped(maxConcurrentCrawls).map { batch =>
          Future.sequence(batch.map { link =>
            crawlAndCategorize(link, depth + 1) // Update depth for recursion
          }).map(_.flatten) // Flatten the result of nested Futures
        }.toSeq

        return Future.sequence(batchedFutures).map { results: Seq[Seq[Content]] =>
          results.flatten // Flatten the results from all batches
        }
      }

      // Use PageFactory to identify the page type
      val page = PageFactory.build(Uri.unsafeParse(resolvedUrl))
      val pageType = page.extract("pageType").str

      // Only process pages of specific types: UserGuidePage, DevGuidePage, or InstanceTypePage
      if (pageType != "UserGuidePage" && pageType != "DevGuidePage" && pageType != "InstanceTypePage") {
        println(s"Skipping page with unsupported type: $pageType")
        return Future.successful(Seq()) // Return an empty Future with a sequence
      }

      // Extract relevant content
      val textContent = SpecificContent.extractText(doc).mkString("\n").replaceAll("[^\\x20-\\x7E]", "") // Clean content
      val codeContent = SpecificContent.extractCode(doc).map(_.toString).mkString("\n").replaceAll("[^\\x20-\\x7E]", "")
      val tableContent = SpecificContent.extractTable(doc).map(_.toString).mkString("\n").replaceAll("[^\\x20-\\x7E]", "")

      // Process current page content
      val content = Content(resolvedUrl, pageType, textContent, tableContent, codeContent, "")
      savePeriodically(Seq(content), pageCount, documentationPath, outputFilename)

      // Batch the links and limit concurrency
      val batchedFutures: Seq[Future[Seq[Content]]] = links.grouped(maxConcurrentCrawls).map { batch =>
        Future.sequence(batch.map { link =>
          crawlAndCategorize(link, depth + 1) // Update depth for recursion
        }).map(_.flatten) // Flatten the result of nested Futures
      }.toSeq

      // Explicitly specify the type expected by Future.sequence and the map operation
      Future.sequence(batchedFutures).map { results: Seq[Seq[Content]] =>
        results.flatten // Flatten the results from all batches
      }

    } catch {
      case e: IllegalArgumentException =>
        println(s"Error processing URL $url: Invalid URL format - ${e.getMessage}")
        Future.successful(Seq()) // Return an empty Future with a sequence
      case e: IOException =>
        println(s"Error processing URL $url: I/O Error - ${e.getMessage}")
        Future.successful(Seq()) // Return an empty Future with a sequence
      case e: Exception =>
        println(s"Error processing URL $url: ${e.getMessage}")
        Future.successful(Seq()) // Return an empty Future with a sequence
    }
  }

  // Start the crawl process
  val crawlFuture = crawlAndCategorize(entrypointDocumentationUrl, 0)

  // Block until crawling is complete
  Await.result(crawlFuture, 10.minutes)

  println(s"Total pages crawled: $pageCount")

  // Function to find links in the HTML content
  override def findLinksInPage(htmlContent: String): Seq[String] = {
    val linkPattern = """<a\s+href=["']([^"']+)["']""".r
    val aLinks = linkPattern.findAllMatchIn(htmlContent).map(_.group(1)).toSeq

    // Extract and decode links from <input> tags
    val inputPattern = """<input[^>]*id=["']landing-page-xml["'][^>]*value=["']([^"']+)["'][^>]*>""".r
    val inputLinks = inputPattern.findAllMatchIn(htmlContent).flatMap { m =>
      val encodedValue = m.group(1)
      try {
        val decodedValue = URLDecoder.decode(encodedValue, "UTF-8")
        val xmlContent = XML.loadString(decodedValue)
        (xmlContent \\ "list-card-item").map(node => (node \ "@href").text)
      } catch {
        case e: Exception =>
          println(s"Error decoding or parsing input tag: ${e.getMessage}")
          Seq()
      }
    }.toSeq

    aLinks ++ inputLinks
  }

  // Function to fetch HTML content from URL
  override def fetchHtmlFromUrl(url: String): String = {
    val backend = HttpURLConnectionBackend()
    val response = basicRequest.get(Uri.unsafeParse(url)).send(backend)
    response.body match {
      case Right(html) => html
      case Left(error) =>
        println(s"Error fetching HTML for URL $url: $error")
        ""
    }
  }
}
