package io.scalastic.aws

import scala.util.{Failure, Success}
import sttp.client3._
import sttp.model.Uri
import org.jsoup.Jsoup
import java.net.URLDecoder
import scala.xml.XML
import java.io.{File, PrintWriter}
import ujson._

object TestContent extends App {

  val rootDocumentationUrl: String = "https://docs.aws.amazon.com"
  val entrypointDocumentationUrl: String = rootDocumentationUrl
  val documentationPath: String = "./data/"
  val outputFilename: String = "TestContent.json"
  val maxDepth = 5 // Set maximum depth for crawling

  var pageCount = 0 // Global counter for the number of pages crawled
  var crawledData = Seq.empty[Content] // Store results as they are fetched

  case class Content(url: String, pageType: String, textContent: String, tableData: String, imageData: String, codeData: String)

  // Create the directory if it doesn't exist
  val directory = new File(documentationPath)
  if (!directory.exists()) {
    println(s"Creating directory: $documentationPath")
    directory.mkdirs()
  } else {
    println(s"Directory already exists: $documentationPath")
  }

  def crawlAndCategorize(url: String, depth: Int): Seq[Content] = {
    if (depth > maxDepth) return Seq() // Stop condition to prevent infinite recursion

    pageCount += 1 // Increment the page count
    println(s"Page Count: $pageCount") // Log the page count

    try {
      val pageUrl: String = if (url.startsWith("http")) url else rootDocumentationUrl + url
      println(s"Processing URL (Depth $depth): $pageUrl")
      val htmlContent = fetchHtmlFromUrl(pageUrl)
      if (htmlContent.isEmpty) {
        println(s"Failed to fetch HTML content from $pageUrl")
        return Seq()
      }

      val doc = Jsoup.parse(htmlContent)

      // Simulate PageFactory functionality
      val pageType = "ExamplePageType" // Replace with your actual logic to extract page type
      println(s"Page Type: $pageType") // Log the content type

      // Extract additional content
      val textContent = SpecificContent.extractText(doc).mkString("\n")
      val tableData = SpecificContent.extractTable(doc).mkString("\n")
      val imageData = SpecificContent.extractImages(doc).mkString(",")
      val codeData = SpecificContent.extractCode(doc).mkString("\n")

      // Create a content object
      val content = Content(pageUrl, pageType, textContent, tableData, imageData, codeData)
      crawledData :+= content

      // Log the content type for debugging
      println(s"Content Type for $pageUrl: $pageType")

      // Save the results periodically (for every 10 pages)
      if (pageCount % 10 == 0) {
        writeResultsToFile(crawledData, documentationPath, outputFilename)
      }

      // Find links in the current page and follow them recursively
      val links = findLinksInPage(htmlContent)
      println(s"Found Links: $links")

      val categorizedLinks = links.flatMap { link =>
        val absoluteLink = if (link.startsWith("http")) link else rootDocumentationUrl + link
        crawlAndCategorize(absoluteLink, depth + 1)
      }

      // Return results for this page and its links
      crawledData

    } catch {
      case e: Exception =>
        println(s"Error processing URL $url: ${e.getMessage}")
        Seq()
    }
  }

  // Start the crawl process
  crawledData = crawlAndCategorize(entrypointDocumentationUrl, 0)
  println(s"Total pages crawled: $pageCount")
  println(s"Crawled data: $crawledData")

  // Write the final results to a JSON file if any data was crawled
  if (crawledData.nonEmpty) {
    writeResultsToFile(crawledData, documentationPath, outputFilename)
  } else {
    println("No data to write")
  }

  // Function to find links in the HTML content
  def findLinksInPage(htmlContent: String): Seq[String] = {
    // Extract links from <a> tags
    val linkPattern = """<a\s+href=["']([^"']+)["']""".r
    val aLinks = linkPattern.findAllMatchIn(htmlContent).map(_.group(1)).toSeq
    println(s"Extracted Links from <a> tags: $aLinks")

    // Extract and decode links from <input> tags
    val inputPattern = """<input[^>]*id=["']landing-page-xml["'][^>]*value=["']([^"']+)["'][^>]*>""".r
    val inputLinks = inputPattern.findAllMatchIn(htmlContent).flatMap { m =>
      val encodedValue = m.group(1)
      try {
        val decodedValue = URLDecoder.decode(encodedValue, "UTF-8")
        println(s"Decoded XML: ${decodedValue.take(200)}") // Print first 200 characters for inspection

        val xmlContent = XML.loadString(decodedValue)
        (xmlContent \\ "list-card-item").map(node => (node \ "@href").text)
      } catch {
        case e: Exception =>
          println(s"Error decoding or parsing XML: ${e.getMessage}")
          Seq()
      }
    }.toSeq
    println(s"Extracted Links from <input> tags: $inputLinks")

    val allLinks = aLinks ++ inputLinks
    println(s"All Extracted Links: $allLinks")
    allLinks
  }

  // Function to fetch HTML content from URL
  def fetchHtmlFromUrl(url: String): String = {
    val backend = HttpURLConnectionBackend()
    val response = basicRequest.get(Uri.unsafeParse(url)).send(backend)
    response.body match {
      case Right(html) => html
      case Left(error) =>
        println(s"Failed to fetch HTML content: $error")
        ""
    }
  }

  // Function to write results to file
  def writeResultsToFile(crawledData: Seq[Content], path: String, filename: String): Unit = {
    val jsonData = ujson.Obj(
      "data" -> ujson.Arr(crawledData.map { case Content(url, pageType, textContent, tableData, imageData, codeData) =>
        ujson.Obj(
          "url" -> url,
          "pageType" -> pageType,
          "textContent" -> textContent,
          "tableData" -> ujson.Arr(tableData.split("\n").map(ujson.Str(_)): _*),
          "imageData" -> ujson.Arr(imageData.split(",").map(ujson.Str(_)): _*),
          "codeData" -> ujson.Str(codeData)
        )
      }: _*)
    )
    val file = new File(path, filename)
    val writer = new PrintWriter(file)
    try {
      writer.write(jsonData.toString())
    } finally {
      writer.close()
    }
  }
}
