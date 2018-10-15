import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5"
  lazy val parserComb = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
  lazy val picoCLI = "info.picocli" % "picocli" % "3.6.1"
  lazy val log = "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0"
  lazy val logBack = "ch.qos.logback" % "logback-classic" % "1.2.3"
  lazy val csv = "com.github.tototoshi" %% "scala-csv" % "1.3.5"
  lazy val nuProcess = "com.zaxxer" % "nuprocess" % "1.2.4"
}
