package io.scalastic.aws

import sttp.model.Uri
import sttp.client3._

case class WebPage(uri: Uri, htmlContent: String) {
  def extract(key: String): ujson.Value = {
    key match {
      case "pageType" => extractPageType()
      case _ => ujson.Null
    }
  }

  private def extractPageType(): ujson.Value = {
    if (htmlContent.isEmpty) {
      ujson.Str("UnknownPage") // Handle empty or missing content
    } else if (htmlContent.contains("UserGuide")) {
      ujson.Str("UserGuidePage")
    } else if (htmlContent.contains("InstanceType")) {
      ujson.Str("InstanceTypePage")
    } else if (htmlContent.contains("DeveloperGuide")) {
      ujson.Str("DevGuidePage")
    } else {
      ujson.Str("MainPage")
    }
  }
}

object WebPageFactory {
  def build(uri: Uri): WebPage = {
    val htmlContent = fetchHtmlFromUri(uri)
    if (htmlContent.isEmpty) {
      println(s"Warning: Failed to fetch content for URI: $uri")
    }
    WebPage(uri, htmlContent)
  }

  def fetchHtmlFromUri(uri: Uri): String = {
    try {
      val backend = HttpURLConnectionBackend()
      val response = basicRequest.get(uri).send(backend)
      response.body match {
        case Right(html) => html
        case Left(error) =>
          println(s"Error fetching HTML for URI $uri: $error")
          ""
      }
    } catch {
      case e: Exception =>
        println(s"Exception fetching HTML for URI $uri: ${e.getMessage}")
        ""
    }
  }
}
