package io.scalastic.aws

import sttp.client3.{UriContext, basicRequest, HttpURLConnectionBackend}
import sttp.model.Uri
import java.io.{File, PrintWriter}
import scala.io.Source
import ujson._
import scala.util.{Try, Success, Failure}

object ClassificationTestv2 extends App with Model {

  val dataPath = "C:\\Users\\petermayer\\OneDrive - Microsoft\\Documents\\Desktop\\scraping_projects\\hello-world-template\\data"
  val outputPath = "data/classification_results_v2.json"
  val logPath = "data/classification_log_v2.txt"

  // List of raw HTML files to classify
  val testFiles: Seq[String] = Seq(
    "raw_html_1.html",
    "raw_html_2.html",
    "raw_html_3.html",
    "raw_html_4.html",
    "raw_html_5.html"
  )

  def testPageClassifier(files: Seq[String]): Unit = {
    val logWriter = new PrintWriter(new File(logPath))

    val classifications = files.map { fileName =>
      logWriter.println(s"Testing File: $fileName")
      Try {
        val filePath = s"$dataPath\\$fileName"
        val htmlContent = Source.fromFile(filePath).mkString
        logWriter.println(s"Read HTML Content from $filePath:\n${htmlContent.take(500)}") // Print the first 500 characters of the HTML content

        val metaTags = extractMetaTags(htmlContent)
        logWriter.println(s"Extracted Meta Tags from $fileName: $metaTags")

        val jsonContent = convertMetaTagsToJson(metaTags)
        logWriter.println(s"Converted Meta Tags to JSON for $fileName:\n$jsonContent")

        val uri = Uri.parse(s"http://example.com/$fileName").getOrElse(throw new IllegalArgumentException("Invalid URL"))
        val pageType = PageFactory.identifyPageType(jsonContent, uri, htmlContent)
        (fileName, pageType.extract("pageType").str)
      } match {
        case Success(result) =>
          result
        case Failure(exception) =>
          logWriter.println(s"Error processing file $fileName: ${exception.getMessage}")
          (fileName, "Error")
      }
    }

    // Save results to JSON file
    val jsonOutput = ujson.Arr(classifications.map { case (fileName, pageType) =>
      ujson.Obj("fileName" -> fileName, "pageType" -> ujson.Str(pageType))
    }: _*)

    val writer = new PrintWriter(new File(outputPath))
    writer.write(jsonOutput.render(indent = 2))
    writer.close()

    logWriter.close()
    println(s"Classification results saved to $outputPath")
    println(s"Detailed log saved to $logPath")
  }

  // Run the test
  testPageClassifier(testFiles)
}
