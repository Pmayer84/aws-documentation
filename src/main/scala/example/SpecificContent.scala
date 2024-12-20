package io.scalastic.aws

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._
import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.ExecutionContext.Implicits.global

object SpecificContent {

  // Function to check if the link is a PDF
  def isPdfLink(link: String): Boolean = {
    link.endsWith(".pdf") // Check if the link ends with ".pdf"
  }

  // Function to check if the page is an AWS Glossary page based on the meta tag
  def isGlossaryPage(content: Document): Boolean = {
    val metaTags = content.select("meta[name=product]").asScala
    metaTags.exists(tag => tag.attr("content") == "AWS Glossary") // Check for meta tag with product="AWS Glossary"
  }

  // Function to check if the page is a Landing Page based on the meta tag
  def isLandingPage(content: Document): Boolean = {
    val metaTags = content.select("meta[name=guide-name][content=Landing Page]").asScala
    metaTags.nonEmpty // Return true if the meta tag is found
  }

  // Extract text content, including links
  def extractText(content: Document): List[String] = {
    val textData = ListBuffer[String]()

    // Skip the page if it's the glossary page or landing page
    if (isGlossaryPage(content) || isLandingPage(content)) {
      return List()  // Skip if the page is the glossary page or landing page
    }

    // Extract main text content, filtering out unwanted elements
    val mainContent = content.select("body").text() // Get all the text inside the <body> tag
    textData += mainContent

    textData.toList
  }

  // Extract table data
  def extractTable(content: Document): List[List[Map[String, String]]] = {
    val tableData = ListBuffer[List[Map[String, String]]]()

    // Skip the page if it's the glossary page or landing page
    if (isGlossaryPage(content) || isLandingPage(content)) {
      return List()  // Skip if the page is the glossary page or landing page
    }

    val tables = content.select("table").asScala
    tables.foreach { table =>
      val headers = table.select("th").asScala.map(_.text()).toList
      val rows = table.select("tr").asScala.drop(1) // Skip header row

      val rowData: List[Map[String, String]] = rows.map { row =>
        val cells = row.select("td").asScala.map(_.text()).toList
        headers.zip(cells).toMap // Convert zipped pairs to a Map
      }.toList

      tableData += rowData
    }

    tableData.toList
  }

  // Extract image sources and descriptions
  def extractImages(content: Document): List[Map[String, String]] = {
    val imageData = ListBuffer[Map[String, String]]()

    // Skip the page if it's the glossary page or landing page
    if (isGlossaryPage(content) || isLandingPage(content)) {
      return List()  // Skip if the page is the glossary page or landing page
    }

    val images = content.select("img").asScala
    images.foreach { img =>
      val src = img.attr("src")
      val alt = img.attr("alt")
      imageData += Map("src" -> src, "alt" -> alt)
    }

    imageData.toList
  }

  // Extract code blocks
  def extractCode(content: Document): List[Map[String, String]] = {
    val codeData = ListBuffer[Map[String, String]]()

    // Skip the page if it's the glossary page or landing page
    if (isGlossaryPage(content) || isLandingPage(content)) {
      return List()  // Skip if the page is the glossary page or landing page
    }

    val codeBlocks = content.select("code, pre").asScala // Select both <code> and <pre> tags
    codeBlocks.foreach { code =>
          val codeText = code.text()
    val langClass = code.className()
    codeData += Map("code" -> codeText, "language" -> langClass)
  }

  codeData.toList
}

// Efficient incremental processing for large documents
def processLargeDocument(url: String): Future[Document] = {
  Future {
    try {
      Jsoup.connect(url).get() // Fetch and parse the document incrementally
    } catch {
      case e: Exception =>
        println(s"Failed to fetch or parse the document at $url: ${e.getMessage}")
        throw e // Rethrow to allow failure handling
    }
  }
}

// Scrape multiple pages concurrently
def scrapePages(urls: List[String]): Future[List[Document]] = {
  Future.sequence(urls.map(url => processLargeDocument(url).recover {
    case e: Exception =>
      println(s"Failed to fetch $url: ${e.getMessage}")
      null // Returning null or empty document as fallback
  }))
}

// Process large table data by handling each row incrementally
def processLargeTable(content: Document): Future[List[List[Map[String, String]]]] = {
  val tableData = ListBuffer[List[Map[String, String]]]()

  // Skip the page if it's the glossary page or landing page
  if (isGlossaryPage(content) || isLandingPage(content)) {
    return Future.successful(List())  // Skip if the page is the glossary page or landing page
  }

  Future {
    val tables = content.select("table").asScala
    val tableData = tables.map { table =>
      val headers = table.select("th").asScala.map(_.text()).toList
      val rows = table.select("tr").asScala.drop(1) // Skip header row

      rows.map { row =>
        val cells = row.select("td").asScala.map(_.text()).toList
        headers.zip(cells).toMap // Convert zipped pairs to a Map
      }.toList
    }.toList

    tableData
  }
}

// Use efficient streaming or chunking for very large documents
def processAndSaveLargeData(content: Document, outputPath: String): Unit = {
  // Skip the page if it's the glossary page or landing page
  if (isGlossaryPage(content) || isLandingPage(content)) return // Skip saving if the page is the glossary page or landing page

  val textData = extractText(content)
  val tableData = extractTable(content)
  val imageData = extractImages(content)
  val codeData = extractCode(content)

  // Process and save each section incrementally to a file or database
  saveDataToFile(outputPath, textData, "text")
  saveDataToFile(outputPath, tableData.map(_.toString), "table")
  saveDataToFile(outputPath, imageData.map(_.toString), "images")
  saveDataToFile(outputPath, codeData.map(_.toString), "code")
}

// Save data to a file in a memory-efficient way
def saveDataToFile(outputPath: String, data: Seq[String], dataType: String): Unit = {
  try {
    val file = new java.io.File(s"$outputPath/$dataType.txt")
    val writer = new java.io.PrintWriter(new java.io.BufferedWriter(new java.io.FileWriter(file, true))) // Append mode
    data.foreach(writer.println)
    writer.close()
  } catch {
    case e: Exception => println(s"Error saving data to file: ${e.getMessage}")
  }
}
}