import sbt._
import Keys._

ThisBuild / scalaVersion := "2.13.9"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "io.scalastic"
ThisBuild / organizationName := "scalastic"

lazy val root = (project in file("."))
  .settings(
    name := "aws-doc-scraper",
    resolvers += "Rally Health" at "https://dl.bintray.com/rallyhealth/maven",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.9" % Test, // Correct ScalaTest dependency
      "com.softwaremill.sttp.client3" %% "core" % "3.8.1",  // Latest version of sttp
      "com.lihaoyi" %% "upickle" % "1.4.0",
      "org.json" % "json" % "20211205",
      "org.scala-lang.modules" %% "scala-xml" % "2.0.1",
      "org.jsoup" % "jsoup" % "1.16.1"
    )
  )
