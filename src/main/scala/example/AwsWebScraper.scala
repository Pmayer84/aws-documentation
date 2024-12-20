package io.scalastic.aws

import io.scalastic.aws.AwsDocumentation.rootDocumentationUrl
import sttp.client3.{HttpURLConnectionBackend, UriContext, basicRequest}
import ujson.Value.InvalidData
import sttp.model.Uri

import scala.util.{Failure, Success, Try}
import java.io.IOException

trait AwsWebScraper extends Model {

  def completeRootDocumentation(v: ujson.Value, pageCount: Int): ujson.Value = v match {
    case a: ujson.Arr =>
      a.arr.map(completeRootDocumentation(_, pageCount))
    case o: ujson.Obj =>
      o.obj.mapValuesInPlace {
        case (k, v) if k == "href" => extractDocumentation(v.str, 0, pageCount) // Pass initial depth 0 and pageCount
        case (k, v) => completeRootDocumentation(v, pageCount)
      }
    case s: ujson.Str => s
    case n: ujson.Num => n
    case _ => Nil
  }

  def extractDocumentation(url: String, depth: Int, pageCount: Int): ujson.Value = {
    if (depth > 5) return ujson.Obj() // Stop condition to prevent infinite recursion

    val newPageCount = pageCount + 1 // Increment the page count
    println(s"Page Count: $newPageCount") // Log the page count

    try {
      val pageUrl: String = if (url.startsWith("http")) url else rootDocumentationUrl + url
      println(s"Processing URL (Depth $depth): $pageUrl")
      val htmlContent = fetchHtmlFromUrl(pageUrl)
      println(s"HTML Content Length: ${htmlContent.length}")
      
      val metaTags = extractMetaTags(htmlContent)
      println(s"Extracted Meta Tags: $metaTags")
      
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

  def extractJson(url: String): ujson.Value = {
    basicRequest
      .get(uri"$url")
      .send(HttpURLConnectionBackend())
      .body match {
      case Right(s) => ujson.read(s)
      case Left(f) => throw InvalidData(ujson.read("{}"), "No JSON found on " + url)
    }
  }

  private def extractField(jsonData: ujson.Value, fieldsList: List[String]): ujson.Value = {
    if (fieldsList.isEmpty) return jsonData

    Try {
      jsonData(fieldsList.head)
    } match {
      case Success(s) => extractField(s, fieldsList.tail)
      case Failure(f) => ujson.Str("Empty " + fieldsList.head)
    }
  }

  // Function to find links in the HTML content
  def findLinksInPage(htmlContent: String): Seq[String] = {
    val linkPattern = """<a\s+href=["']([^"']+)["']""".r
    linkPattern.findAllMatchIn(htmlContent).map(_.group(1)).toSeq
  }
}
