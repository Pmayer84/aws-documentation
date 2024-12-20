package io.scalastic.aws

import java.io.{FileInputStream, ObjectInputStream}
import ujson.Value

object DeserializeSerFiles {

  // Custom ObjectInputStream with ClassLoader
  class ObjectInputStreamWithCustomClassLoader(fileInputStream: FileInputStream)
    extends ObjectInputStream(fileInputStream) {
    override def resolveClass(desc: java.io.ObjectStreamClass): Class[_] = {
      try {
        Class.forName(desc.getName, false, getClass.getClassLoader)
      } catch {
        case ex: ClassNotFoundException => super.resolveClass(desc)
      }
    }
  }

  // Method to deserialize the object
  def deserialize(filePath: String): Option[Value] = {
    try {
      val ois = new ObjectInputStreamWithCustomClassLoader(new FileInputStream(filePath))
      val obj = ois.readObject().asInstanceOf[Value]
      ois.close()
      Some(obj)
    } catch {
      case ex: Exception =>
        println(s"Error deserializing file $filePath: ${ex.getMessage}")
        None
    }
  }

  // Main method to test deserialization
  def main(args: Array[String]): Unit = {
    val filePaths = List(
      "./data/root-documentation.ser",
      "./data/full-documentation.ser"
    )

    filePaths.foreach { filePath =>
      println(s"Deserializing file: $filePath")
      val obj = deserialize(filePath)
      obj match {
        case Some(data) => println(ujson.write(data, 2))
        case None => println(s"Failed to deserialize file: $filePath")
      }
    }
  }
}
