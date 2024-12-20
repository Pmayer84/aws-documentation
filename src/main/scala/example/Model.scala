package io.scalastic.aws


import sttp.client3.{UriContext, basicRequest, HttpURLConnectionBackend}
import sttp.model.Uri
import java.io._
import java.net.URLDecoder
import java.nio.charset.Charset
import ujson._
import org.json.XML
import scala.util.Try

trait Model {

  val sort: Option[String] = None
  val query: String = "http language:scala"
  val xmlVariable = "landing-page-xml"

  // Fetch HTML content from URL
  def fetchHtmlFromUrl(url: String): String = {
    val request = basicRequest.get(uri"$url")
    val backend = HttpURLConnectionBackend()
    val response = request.send(backend)

    val actualContentType = response.contentType.getOrElse("unknown")
    val expectedContentType = "text/html"

    println(s"Expected Content Type: $expectedContentType")
    println(s"Actual Content Type: $actualContentType")

    if (actualContentType.startsWith(expectedContentType)) {
      response.body match {
        case Right(content) =>
          if (content.startsWith("<!doctype html>") || content.startsWith("<html")) {
            content
          } else {
            throw new Exception(s"Unexpected content type for $url: $content")
          }
        case Left(error) => throw new Exception(s"Error fetching HTML for $url: $error")
      }
    } else {
      throw new Exception(s"Unsupported content type for $url: $actualContentType")
    }
  }

  // Extract meta tags from HTML content
  def extractMetaTags(htmlContent: String): Map[String, String] = {
    val metaTagPattern = """<meta\s+name=["']([^"']+)["']\s+content=["']([^"']+)["']\s*/?>""".r
    metaTagPattern.findAllMatchIn(htmlContent).map { m =>
      (m.group(1), m.group(2))
    }.toMap
  }

  // Convert meta tags to JSON
  def convertMetaTagsToJson(metaTags: Map[String, String]): ujson.Value = {
    val jsonObj = ujson.Obj()
    metaTags.foreach { case (key, value) =>
      jsonObj(key) = ujson.Str(value)
    }
    jsonObj
  }

  trait Page {
    val uri: Uri
    def extract: ujson.Value
  }

  class WebPage(val uri: Uri) extends Page {
    override def extract: ujson.Value = ujson.Obj("pageType" -> "WebPage")
  }

  class MainPage(uri: Uri) extends WebPage(uri) {
    override def extract: ujson.Value = ujson.Obj("pageType" -> "MainPage")
  }

  class SubPage(uri: Uri) extends WebPage(uri) {
    override def extract: ujson.Value = ujson.Obj("pageType" -> "SubPage")
  }

  class UserGuidePage(uri: Uri) extends WebPage(uri) {
    override def extract: ujson.Value = ujson.Obj("pageType" -> "UserGuidePage")
  }

  class DevGuidePage(uri: Uri) extends WebPage(uri) {
    override def extract: ujson.Value = ujson.Obj("pageType" -> "DevGuidePage")
  }

  class InstanceTypePage(uri: Uri) extends WebPage(uri) {
    override def extract: ujson.Value = ujson.Obj("pageType" -> "InstanceTypePage")
  }

  class UnknownPage(uri: Uri) extends WebPage(uri) {
    override def extract: ujson.Value = ujson.Obj("pageType" -> "UnknownPage")
  }

  object PageFactory {
    def build(uri: Uri): WebPage = {
      val htmlContent = fetchHtmlFromUrl(uri.toString)
      val metaTags = extractMetaTags(htmlContent)
      println(s"Extracted Meta Tags: $metaTags")
      val jsonContent = convertMetaTagsToJson(metaTags)

      identifyPageType(jsonContent, uri, htmlContent)
    }

    def identifyPageType(jsonContent: ujson.Value, uri: Uri, htmlContent: String): WebPage = {
      val metaTags = jsonContent.obj
      val metaServiceName = metaTags.getOrElse("service-name", ujson.Str("")).str
      val metaGuideName = metaTags.getOrElse("guide-name", ujson.Str("")).str
      val metaGuide = metaTags.getOrElse("guide", ujson.Str("")).str
      val metaTargetState = metaTags.getOrElse("target_state", ujson.Str("")).str

      println(s"metaTags for URI $uri: $metaTags")
      println(s"metaServiceName: $metaServiceName")
      println(s"metaGuideName: $metaGuideName")
      println(s"metaGuide: $metaGuide")
      println(s"metaTargetState: $metaTargetState")

      // Enhanced identification logic
      if (metaServiceName.contains("Main Landing Page") && metaGuideName.contains("Landing Page")) {
        println(s"Identified as MainPage for URI: $uri")
        new MainPage(uri)
      } else if (metaServiceName.contains("Documentation") && metaGuideName.contains("Landing Page")) {
        println(s"Identified as SubPage for URI: $uri")
        new SubPage(uri)
      } else if (metaGuide.contains("User Guide")) {
        println(s"Identified as UserGuidePage for URI: $uri")
        new UserGuidePage(uri)
      } else if (metaGuide.contains("Developer Guide")) {
        println(s"Identified as DevGuidePage for URI: $uri")
        new DevGuidePage(uri)
      } else if (metaTargetState.contains("instance-types")) {
        println(s"Identified as InstanceTypePage for URI: $uri")
        new InstanceTypePage(uri)
      } else if (htmlContent.contains("Amazon Comprehend")) { // Additional logic for fallback
        println(s"Identified as WebPage for URI: $uri based on content")
        new WebPage(uri)
      } else {
        println(s"Identified as UnknownPage for URI: $uri")
        new UnknownPage(uri)
      }
    }
  }
}
