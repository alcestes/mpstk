import Dependencies._

// Find rt.jar.  Kudos to:
// https://stackoverflow.com/a/31322970
val rtJar: String = System.getProperty("sun.boot.class.path")
  .split(java.io.File.pathSeparator).collectFirst {
    case str: String if str.endsWith(java.io.File.separator + "rt.jar") => str
  }.get // Fail hard if not found

// Determine the Java version number, for linking Java API docs
val javaVersion: String = {
  System.getProperty("java.specification.version").split("\\.").last
} // Fail hard if the version format is not 1.xx

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "uk.ac.ic.doc.mrg",
      organizationHomepage := Some(url("http://mrg.doc.ic.ac.uk")),
      organizationName := "Mobility Reading Group",
      scalaVersion := "2.12.7",
      version := "0.1.0-SNAPSHOT"
    )),
    name := "mpstk",
    description := "A toolkit to define protocols as Multiparty Session Types, and verify their properties",

    libraryDependencies ++= Seq(
      parserComb,
      scalaTest % Test,
      picoCLI,
      log, logBack,
      csv,
      nuProcess
    ),

    scalacOptions ++= Seq(
      "-deprecation",
      "-unchecked",
      "-feature"
    ),

    scalacOptions in (Compile, doc) += s"-doc-external-doc:${rtJar}#http://docs.oracle.com/javase/${javaVersion}/docs/api",

    // Generate a CLASSPATH file in the target directory
    sourceGenerators in Compile += Def.task {
      val cp: Seq[File] = {
        Seq((classDirectory in Compile).value) ++
          (externalDependencyClasspath in Compile).value.files
      }
      val file = (target in Compile).value / "CLASSPATH"
      val sep = System.getProperties().getProperty("path.separator")
      IO.write(file, cp.map(_.toString).mkString(sep).getBytes)
      Seq() // Do not return any new source file
    }.taskValue
  )
