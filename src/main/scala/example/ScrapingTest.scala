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

object ScrapingTest extends AwsWebScraper with IO with Model with App {

  val documentationPath: String = "./data/"
  val outputFilename: String = "ScraperTest.json"

  // List of URLs to scrape
  val urlsToScrape: List[String] = List(
    "https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/concepts.html",
    "https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet-configuration-strategies.html",
    "https://docs.aws.amazon.com/autoscaling/ec2/userguide/example_auto-scaling_Hello_section.html"
    // Add more URLs as needed
  )

  // Create the directory if it doesn't exist
  val directory = new File(documentationPath)
  if (!directory.exists()) {
    println(s"Creating directory: $documentationPath")
    directory.mkdirs()
  } else {
    println(s"Directory already exists: $documentationPath")
  }

  def normalizeUrl(url: String): String = {
    if (url.startsWith("http")) url else "https://docs.aws.amazon.com" + url
  }

  def scrapeAndExtract(url: String): Future[Seq[Content]] = {
    val resolvedUrl = normalizeUrl(url)
    println(s"Scraping URL: $resolvedUrl")

    try {
      val htmlContent = fetchHtmlFromUrl(resolvedUrl)
      if (htmlContent.isEmpty) return Future.successful(Seq())

      val doc = Jsoup.parse(htmlContent)

      // Skip pages without body content or misleading titles
      if (doc == null || doc.select("body").isEmpty || doc.title().equalsIgnoreCase("PDF Document")) {
        println(s"Skipping page without body content or PDF document: $resolvedUrl")
        return Future.successful(Seq())
      }

      // Detect and skip pages with embedded PDF content
      val embedTags = doc.select("embed[type=application/x-google-chrome-pdf]")
      if (!embedTags.isEmpty) {
        println(s"Skipping page with embedded PDF content: $resolvedUrl")
        return Future.successful(Seq())
      }

      // Check for meta tags indicating the page should be skipped
      val isDecisionGuide = doc.select("meta[name=guide][content=AWS Decision Guide]").size() > 0
      val isLandingPage = doc.select("meta[name=guide-name][content=Landing Page]").size() > 0
      if (isDecisionGuide || isLandingPage) {
        println(s"Skipping scraping content due to meta tag: $resolvedUrl")
        return Future.successful(Seq())
      }

      // Use PageFactory to identify the page type
      val page = PageFactory.build(Uri.unsafeParse(resolvedUrl))
      val pageType = page.extract("pageType").str

      // Only process pages of specific types: UserGuidePage, DevGuidePage, or InstanceTypePage
      if (pageType != "UserGuidePage" && pageType != "DevGuidePage" && pageType != "InstanceTypePage") {
        println(s"Skipping page with unsupported type: $pageType")
        return Future.successful(Seq())
      }

      // Extract relevant content
      val textContent = TestContentExtractor.extractTextTest(doc).mkString("\n").replaceAll("[^\\x20-\\x7E]", "") // Clean content
      val codeContent = TestContentExtractor.extractCodeTest(doc).map(_.toString).mkString("\n").replaceAll("[^\\x20-\\x7E]", "")
      val tableContent = TestContentExtractor.extractTableTest(doc).map(_.toString).mkString("\n").replaceAll("[^\\x20-\\x7E]", "")

      // Process current page content
      val content = Content(resolvedUrl, pageType, textContent, tableContent, codeContent, "")
      savePeriodically(Seq(content), urlsToScrape.indexOf(url), documentationPath, outputFilename)

      Future.successful(Seq(content))

    } catch {
      case e: IllegalArgumentException =>
        println(s"Error processing URL $url: Invalid URL format - ${e.getMessage}")
        Future.successful(Seq())
      case e: IOException =>
        println(s"Error processing URL $url: I/O Error - ${e.getMessage}")
        Future.successful(Seq())
      case e: Exception =>
        println(s"Error processing URL $url: ${e.getMessage}")
        Future.successful(Seq())
    }
  }


  // Start the scraping process for the provided URLs
  val scrapingFutures: Seq[Future[Seq[Content]]] = urlsToScrape.map(scrapeAndExtract)

  // Wait for all scraping tasks to complete
  val combinedFuture: Future[Seq[Content]] = Future.sequence(scrapingFutures).map(_.flatten)

  // Block until all scraping is complete
  Await.result(combinedFuture, 10.minutes)

  println(s"Scraping complete. Total URLs scraped: ${urlsToScrape.size}")

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
