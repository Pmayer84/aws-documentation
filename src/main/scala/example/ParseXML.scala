package io.scalastic.aws

import sttp.client3._
import java.net.URLDecoder
import scala.xml.XML

object ParseXML extends App {

  // URL of the page to be parsed
  val rootDocumentationUrl: String = "https://docs.aws.amazon.com"

  // Fetch HTML content from the URL
  def fetchHtml(url: String): String = {
    val backend = HttpURLConnectionBackend()
    val response = basicRequest.get(uri"$url").send(backend)
    response.body match {
      case Right(html) => html
      case Left(error) =>
        println(s"Failed to fetch HTML content: $error")
        ""
    }
  }

  // Extract and decode value from <input> tag, then parse as XML and extract links
  def extractLinks(htmlContent: String): Seq[String] = {
    // Pattern to match <input> tag with id="landing-page-xml"
    val inputPattern = """<input[^>]*id=["']landing-page-xml["'][^>]*value=["']([^"']+)["'][^>]*>""".r
    val inputLinks = inputPattern.findAllMatchIn(htmlContent).flatMap { m =>
      val encodedValue = m.group(1)
      val decodedValue = URLDecoder.decode(encodedValue, "UTF-8")
      val xmlContent = XML.loadString(decodedValue)
      (xmlContent \\ "list-card-item").map(node => (node \ "@href").text)
    }.toSeq
    println(s"Extracted Links from <input> tag: $inputLinks")

    // Pattern to match <a> tags
    val linkPattern = """<a\s+href=["']([^"']+)["']""".r
    val aLinks = linkPattern.findAllMatchIn(htmlContent).map(_.group(1)).toSeq
    println(s"Extracted Links from <a> tags: $aLinks")

    // Combine both sets of links
    val allLinks = aLinks ++ inputLinks
    println(s"All Extracted Links: $allLinks")
    allLinks
  }

  // Main function to perform the extraction
  def main(url: String): Unit = {
    val htmlContent = fetchHtml(url)
    if (htmlContent.nonEmpty) {
      val links = extractLinks(htmlContent)
      println(s"Extracted Links: $links")
    } else {
      println("Failed to fetch or parse HTML content")
    }
  }

  // Execute the main function with the root URL
  main(rootDocumentationUrl)
}
