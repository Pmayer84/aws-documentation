package io.scalastic.aws

import sttp.model.Uri
import java.io.{File, PrintWriter}
import scala.util.{Try, Success, Failure}

object UrlTest extends App {

  val rootDocumentationUrl: String = "https://docs.aws.amazon.com"
  val outputPath = "data/url_test_output.txt" // Unique name for the log file

  // Extended list of URLs to test
  val testUrls: Seq[String] = Seq(
    "https://docs.aws.amazon.com",
    "/dynamodb/?icmpid=docs_homepage_featuredsvcs",
    "/chime/?icmpid=docs_homepage_busapp",
    "/invalid:url",
    "https://notarealwebsite.xyz",
    "https://www.example.com",
    "/path/to/resource",
    "invalid:characters/in/url",
    "https://docs.aws.amazon.com/path?query=param",
    "https://anotherexample.com/with/path",
    // Additional URLs to reach up to 100
    "https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/concepts.html",
    "https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/amazon-ec2-managed-instances.html",
    "https://docs.aws.amazon.com/augmented-ai/?icmpid=docs_homepage_ml",
    "https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-use-augmented-ai-a2i-human-review-loops.html?icmpid=docs_a2i_lp",
    "https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-start-human-loop.html",
    "https://aws.amazon.com/getting-started/guides/setup-environment/",
    "https://docs.aws.amazon.com/machine-learning/",
    "https://docs.aws.amazon.com/comprehend/latest/dg/what-is.html",
    // ... (Repeat similar entries to reach 100 URLs)
  ).take(100) // Ensure only 100 URLs are tested

  def constructAbsoluteUrl(url: String): String = {
    val absoluteUrl = if (url.startsWith("http")) url else rootDocumentationUrl + url
    println(s"Constructed URL: $absoluteUrl")
    absoluteUrl
  }

  // Function to validate URLs using URI class
  def isValidUrl(url: String): Boolean = {
    try {
      new java.net.URI(url)
      true
    } catch {
      case _: Exception => false
    }
  }

  def testUrlsAndLogResults(): Unit = {
    val logWriter = new PrintWriter(new File(outputPath))

    testUrls.foreach { url =>
      try {
        val constructedUrl = constructAbsoluteUrl(url)
        val valid = isValidUrl(constructedUrl)
        logWriter.println(s"Valid URL: $valid | Constructed URL: $constructedUrl")
      } catch {
        case e: IllegalArgumentException =>
          logWriter.println(s"Error constructing URL $url: Invalid URL format - ${e.getMessage}")
        case e: Exception =>
          logWriter.println(s"Error constructing URL $url: ${e.getMessage}")
      }
    }

    logWriter.close()
    println(s"URL validation results saved to $outputPath")
  }

  // Run the test
  testUrlsAndLogResults()
}
