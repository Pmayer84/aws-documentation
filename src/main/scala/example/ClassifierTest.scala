package io.scalastic.aws

import sttp.client3.{UriContext, basicRequest, HttpURLConnectionBackend}
import sttp.model.Uri
import java.io.{File, PrintWriter}
import ujson._
import scala.util.{Try, Success, Failure}

object ClassifierTest extends App with Model {

  val outputPath = "data/classification_results.json"
  val logPath = "data/classification_log.txt"

  // Placeholder list of URLs to classify
  val testUrls: Seq[String] = Seq(
    "https://docs.aws.amazon.com/",
    "https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/concepts.html",
    "https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet-configuration-strategies.html",
    "https://docs.aws.amazon.com/autoscaling/ec2/userguide/example_auto-scaling_Hello_section.html",
    "https://docs.aws.amazon.com/augmented-ai/?icmpid=docs_homepage_ml",
    "https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-use-augmented-ai-a2i-human-review-loops.html?icmpid=docs_a2i_lp",
    "https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-start-human-loop.html",
    "https://aws.amazon.com/getting-started/guides/setup-environment/",
    "https://docs.aws.amazon.com/machine-learning/",
    "https://docs.aws.amazon.com/comprehend/latest/dg/what-is.html"
  )

  def testPageClassifier(urls: Seq[String]): Unit = {
    val logWriter = new PrintWriter(new File(logPath))

    val classifications = urls.map { url =>
      logWriter.println(s"Testing URL: $url")
      Try {
        val uri = Uri.parse(url).getOrElse(throw new IllegalArgumentException("Invalid URL"))
        val backend = HttpURLConnectionBackend()
        val response = basicRequest.get(uri).send(backend)
        val actualContentType = response.contentType.getOrElse("unknown")

        logWriter.println(s"Headers for $url: ${response.headers}")
        logWriter.println(s"Expected Content Type: text/html")
        logWriter.println(s"Actual Content Type: $actualContentType")

        if (!actualContentType.startsWith("text/html")) {
          throw new Exception(s"Unsupported content type for $url: $actualContentType")
        }

        val htmlContent = response.body match {
          case Right(content) =>
            logWriter.println(s"Fetched HTML Content for $url:\n${content.take(500)}") // Print the first 500 characters of the HTML content
            content
          case Left(error) => throw new Exception(s"Error fetching HTML for $url: $error")
        }

        val metaTags = extractMetaTags(htmlContent)
        logWriter.println(s"Extracted Meta Tags for $url: $metaTags")

        val jsonContent = convertMetaTagsToJson(metaTags)
        logWriter.println(s"Converted Meta Tags to JSON for $url:\n$jsonContent")

        val pageType = PageFactory.identifyPageType(jsonContent, uri, htmlContent)
        (url, pageType.extract("pageType").str)
      } match {
        case Success(result) =>
          result
        case Failure(exception) =>
          logWriter.println(s"Error processing URL $url: ${exception.getMessage}")
          (url, "Error")
      }
    }

    // Save results to JSON file
    val jsonOutput = ujson.Arr(classifications.map { case (url, pageType) =>
      ujson.Obj("url" -> url, "pageType" -> ujson.Str(pageType))
    }: _*)

    val writer = new PrintWriter(new File(outputPath))
    writer.write(jsonOutput.render(indent = 2))
    writer.close()

    logWriter.close()
    println(s"Classification results saved to $outputPath")
    println(s"Detailed log saved to $logPath")
  }

  // Run the test
  testPageClassifier(testUrls)
}
