package io.scalastic.aws

import scala.util.{Failure, Success}
import sttp.model.Uri
import java.io.{File, IOException}
import sttp.client3.UriContext

object AwsDocumentation extends AwsWebScraper with Serializer with IO with Utils with App {

  val rootDocumentationUrl: String = "https://docs.aws.amazon.com"
  val entrypointDocumentationUrl: String = rootDocumentationUrl

  val documentationPath: String = "./data/"
  val rootDocumentationFilename: String = "root-documentation"
  val fullDocumentationFilename: String = "full-documentation"

  var pageCount = 0 // Global counter for the number of pages crawled

  // Create the directory if it doesn't exist
  val directory = new File(documentationPath)
  if (!directory.exists()) {
    directory.mkdirs()
  }

  // 1. Get AWS root documentation entries
  val rootDocumentation: ujson.Value = deserialize(rootDocumentationFilename, classOf[ujson.Value]) match {
    case Success(s: ujson.Value) => 
      println("Successfully deserialized root documentation")
      s
    case Failure(f) => 
      println(s"Failed to deserialize root documentation: $f")
      val doc = extractDocumentation(entrypointDocumentationUrl, 0, pageCount)
      serialize(rootDocumentationFilename, doc)
      doc
  }

  // Print JSON content for debugging
  println(s"Root Documentation JSON Content: ${ujson.write(rootDocumentation, 2)}")

  // 2. Complete documentation with href content from root documentation
  val fullDocumentation: ujson.Value = deserialize(fullDocumentationFilename, classOf[ujson.Value]) match {
    case Success(s: ujson.Value) => 
      println("Successfully deserialized full documentation")
      s
    case Failure(f) => 
      println(s"Failed to deserialize full documentation: $f")
      completeRootDocumentation(rootDocumentation, pageCount)
      serialize(fullDocumentationFilename, rootDocumentation)
      rootDocumentation
  }

  // 3. Adds internal IDs to easily manage array data on UI components, correct relative URL links.
  enhancer(fullDocumentation, "main")

  // 4. Write resulting JSON to file
  println(s"Final Full Documentation JSON Content: ${ujson.write(fullDocumentation, 2)}")
  write(fullDocumentationFilename, fullDocumentation)

  // Recursive function to extract documentation and follow links up to a specified depth
  override def extractDocumentation(url: String, depth: Int, pageCount: Int): ujson.Value = {
    if (depth > 5) return ujson.Obj() // Stop condition to prevent infinite recursion

    val newPageCount = pageCount + 1 // Increment the page count
    println(s"Page Count: $newPageCount") // Log the page count

    try {
      val pageUrl: String = if (url.startsWith("http")) url else rootDocumentationUrl + url
      println(s"Processing URL (Depth $depth): $pageUrl")
      val htmlContent = fetchHtmlFromUrl(pageUrl)
      if (htmlContent.isEmpty) {
        println(s"Failed to fetch HTML content from $pageUrl")
        return ujson.Obj()
      }
      println(s"HTML Content Length: ${htmlContent.length}")
      
      val metaTags = extractMetaTags(htmlContent)
      println(s"Extracted Meta Tags: $metaTags")
      
      if (metaTags.isEmpty) {
        println(s"No meta tags found for $pageUrl")
        return ujson.Obj()
      }

      val jsonContent = convertMetaTagsToJson(metaTags)
      println(s"Converted JSON Content: ${ujson.write(jsonContent, 2)}")

      // Create a page instance using PageFactory and extract its content
      val page: Page = PageFactory.build(Uri(pageUrl))
      val extractedContent = page.extract

      // Log extracted content
      println(s"Extracted Content for URL: $pageUrl")
      println(s"${ujson.write(extractedContent, 2)}")

      // Find links in the current page and follow them recursively
      val links = findLinksInPage(htmlContent)
      println(s"Found Links: $links")

      links.foreach { link =>
        val linkedContent = extractDocumentation(link, depth + 1, newPageCount)
        // Add linked content to the extracted content (you can adjust this logic as needed)
        extractedContent.obj(link) = linkedContent
      }

      extractedContent
    } catch {
      case e: IllegalArgumentException =>
        println(s"Error processing URL $url: Invalid URL format - ${e.getMessage}")
        ujson.Obj()
      case e: IOException =>
        println(s"Error processing URL $url: I/O Error - ${e.getMessage}")
        ujson.Obj()
      case e: Exception =>
        println(s"Error processing URL $url: ${e.getMessage}")
        ujson.Obj()
    }
  }

  // Function to find links in the HTML content
  override def findLinksInPage(htmlContent: String): Seq[String] = {
    val linkPattern = """<a\s+href=["']([^"']+)["']""".r
    linkPattern.findAllMatchIn(htmlContent).map(_.group(1)).toSeq
  }
}
