package io.scalastic.aws

import java.io._
import java.net.URLDecoder
import java.nio.charset.Charset
import scala.util.matching.Regex

object HtmlUtils {

  def readHtmlFromFile(dir: String, fileName: String): String = {
    val file = new File(dir, fileName)
    val source = scala.io.Source.fromFile(file)
    val content = try source.mkString finally source.close()
    content
  }

  def extractMetaTags(htmlContent: String): Map[String, String] = {
    val metaTagPattern = """<meta\s+name=["']([^"']+)["']\s+content=["']([^"']+)["']\s*/?>""".r
    metaTagPattern.findAllMatchIn(htmlContent).map { m =>
      (m.group(1), m.group(2))
    }.toMap
  }

  def convertMetaTagsToJson(metaTags: Map[String, String]): ujson.Value = {
    val jsonObj = ujson.Obj()
    metaTags.foreach { case (key, value) =>
      jsonObj(key) = ujson.Str(value)
    }
    jsonObj
  }
}
