package io.scalastic.aws

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable
import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.ExecutionContext.Implicits.global

object PeriodicSave {

  case class Content(url: String, pageType: String, title: String, body: String, code: String, table: String)

  // Use a thread-safe structure for concurrent updates
  private val results = new mutable.ListBuffer[Content]()

  def savePeriodically(contents: Seq[Content], pageCount: Int, path: String, outputFilename: String): Unit = {
    // Filter out content related to embedded PDFs, PGP keys, or excessive unicode
    val filteredContents = contents.filterNot(content => {
      content.url.contains(".pdf") || 
      content.url.contains("chrome-extension://") || 
      containsEmbeddedPdf(content.body) || 
      containsPGPKey(content.body) || 
      containsExcessiveUnicode(content.body)
    })

    results ++= filteredContents

    // Async write to avoid blocking the main flow
    Future {
      writeResultsToFile(path, outputFilename)
    }
  }

  // Helper function to check for embedded PDFs in the body
  def containsEmbeddedPdf(body: String): Boolean = {
    body.contains("<embed") || body.contains("<object")
  }

  // Helper function to check for PGP keys in the body
  def containsPGPKey(body: String): Boolean = {
    body.contains("-----BEGIN PGP PUBLIC KEY BLOCK-----") || body.contains("-----END PGP PUBLIC KEY BLOCK-----")
  }

  // Helper function to check for excessive unicode in the body
  def containsExcessiveUnicode(body: String): Boolean = {
    body.exists(ch => ch > 127)
  }

  def writeResultsToFile(path: String, outputFilename: String): Unit = {
    try {
      val file = new File(path + outputFilename)
      val writer = new BufferedWriter(new FileWriter(file, true)) // Use append mode to avoid overwriting

      // Lock the buffer while writing to avoid concurrency issues
      synchronized {
        results.foreach { content =>
          val json = ujson.Obj(
            "url" -> content.url,
            "pageType" -> content.pageType,
            "title" -> wrapText(escapeSpecialChars(content.title)),
            "body" -> wrapText(escapeSpecialChars(content.body)),
            "code" -> wrapText(escapeSpecialChars(content.code)),
            "table" -> wrapText(escapeSpecialChars(content.table))
          )
          writer.write(json.render(indent = 2) + "\n") // Indent for readability
        }

        // Clear the results buffer after saving to free up memory
        results.clear()
      }

      writer.close()
    } catch {
      case e: Exception =>
        println(s"Error writing results to file: ${e.getMessage}")
        e.printStackTrace()  // Provide stack trace for better debugging
    }
  }

  // Helper function to escape special characters in JSON strings
  def escapeSpecialChars(text: String): String = {
    text.replace("\\", "\\\\")
        .replace("\"", "\\\"")
        .replace("\b", "\\b")
        .replace("\f", "\\f")
        .replace("\n", "\\n")
        .replace("\r", "\\r")
        .replace("\t", "\\t")
  }

  // Helper function to wrap text for readability
  def wrapText(text: String, wrapLength: Int = 80): String = {
    // Replace newlines with a placeholder to keep the structure intact
    val preservedText = text.replace("\n", "\n\n")
    
    // Wrap text to ensure lines break at wrapLength
    val wrappedText = preservedText.grouped(wrapLength).mkString("\n")
    
    // Restore newlines where they were originally
    wrappedText.replace("\n\n", "\n")
  }
}

