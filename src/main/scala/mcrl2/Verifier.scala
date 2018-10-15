// mpstk - the Multiparty Session Types toolKit
// Copyright 2018 Alceste Scalas <alceste.scalas @ imperial.ac.uk>
// Released under the MIT License: https://opensource.org/licenses/MIT
package mpstk.mcrl2

import mpstk._

import com.typesafe.scalalogging.LazyLogging

import java.io.{BufferedReader, InputStreamReader}
import java.nio.file.{Files, Path, Paths}

/** Playground to verify whether a set of [[mpstk.mcrl2.Spec]]s satisfy a
  * set of [[mpstk.mcrl2.Property]]. */
protected[mpstk]
class Verifier(val spec: Spec,
               val properties: Set[Property]) extends LazyLogging {
  lazy val tempDir: Path = {
    val dir = Files.createTempDirectory("mpstk-")
    logger.debug(s"New temporary directory: ${dir}")
    dir
  }

  private
  lazy val specFile: Path = {
    val path = tempDir.resolve(Paths.get(spec.filename ++ ".mcrl2"))
    logger.debug(s"Creating mCRL2 specification file: ${path}")
    Files.write(path, spec.show.getBytes)
  }

  private
  lazy val lpsFile: Path = {
    val path = tempDir.resolve(Paths.get(spec.filename ++ ".lps"))
    logger.debug(s"Generating LPS file: ${path}")
    val cmd = "mcrl22lps"
    val args = Seq(s"${specFile}", s"${path}")
    util.runCommand(cmd, args)
    path
  }

  private
  lazy val ltsFile: Path = {
    val path = tempDir.resolve(Paths.get(spec.filename ++ ".lts"))
    logger.debug(s"Generating LTS file: ${path}")
    val cmd = "lps2lts"
    val args = Seq(s"${lpsFile}", s"${path}")
    util.runCommand(cmd, args)
    path
  }

  /** The number of states of the mCRL2 specification {@code spec}. */
  lazy val states: Long = {
    ltsinfoResult(util.runCommand("ltsinfo", Seq(s"${ltsFile}")))
  }

  private
  lazy val mcfFiles: Map[Property, Path] = Map(properties.map { p =>
    val path = tempDir.resolve(Paths.get(p.filename ++ ".mcf"))
    logger.debug(s"Creating MCF file: ${path.getFileName}")
    (p, Files.write(path, p.show.getBytes))
  }.toSeq:_*)

  private
  lazy val pbesFiles: Map[Property, Path] = Map(properties.map { p =>
    val path = tempDir.resolve(Paths.get(s"${spec.filename}-${p.filename}.pbes"))
    logger.debug(s"Creating PBES file: ${path.getFileName}")
    val cmd = "lps2pbes"
    val args = Seq("-f", s"${mcfFiles(p)}", s"${lpsFile}", s"${path}")
    util.runCommand(cmd, args)
    (p, path)
  }.toSeq:_*)

  // Return the mCRL2 command that verifies the parameterised boolean
  // equation system (PBES) of the spec, for the given property
  private def pbesCmdArgs(p: Property): (String, Seq[String]) = {
    ("pbes2bool", Seq("--strategy=2", s"${pbesFiles(p)}"))
  }

  /** Verification results */
  lazy val results: Map[Property, Boolean] = Map(properties.map { p =>
    logger.debug(s"Checking: ${pbesFiles(p)}")
    val (cmd, args) = pbesCmdArgs(p)
    (p, pbesResult(util.runCommand(cmd, args)))
  }.toSeq:_*)

  /** Benchmark verification time, with {@code reps} repetitions.
    *
    * @param realTime return the execution time of mCRL2's pbes2bool tool
    *                 using the {@code time} utility
    */
  def benchmark(reps: Int,
                realTime: Boolean = false): Map[Property, Seq[Long]] = Map(
    properties.map { p =>
      logger.debug(s"Benchmarking: ${pbesFiles(p)}")
      val (cmd, args) = pbesCmdArgs(p)
      val bench: Any => Long = if (realTime) {
        logger.debug(s"Using 'time' utility on: ${cmd} ${args.mkString(" ")}")
        _ => {
          // Use "-p" option for POSIX-compatible output
          val pt = util.runCommand("time", Seq("-p", cmd) ++ args)
          // We need the 3rd line from the end of stderr
          val res = pt.stderr.split("\n").takeRight(3)(0)
          logger.debug(s"Execution time: '${res}'")
          // We support both "0.12" and "0,12" (poor man's i18n attempt)
          val timeRegex = raw"real (\d+)[\.,](\d+)$$".r
          res match {
            case timeRegex(secs, cents) => {
              // secs are seconds; cents are 1/100s of a second
              (secs.toLong * 1000000000) + (cents.toLong * 10000000)
            }
          }
        }
      } else {
        _ => util.runCommand(cmd, args).nanosecs
      }
      (p, (1 to reps).map(bench))
    }.toSeq:_*)

  /** Close the verifier.
    *
    * This method disposes of all the verifier resources (e.g., its
    * temporary files). After invoking this method, the verifier
    * behaviour is undefined.
    */
  def close(): Unit = {
    logger.debug(s"Removing temporary dir: ${tempDir}")
    deleteDir(tempDir.toFile)
  }

  // Given the result of a pbes2bool invocation (via util.runCommand),
  // parse the process output and return the verification outcome (true/false)
  private def pbesResult(pt: util.StdOutErrAndTime): Boolean = {
    pt.stdout.split("\n")(0) match {
      case "true"  => true
      case "false" => false
      case _       => {
        throw new RuntimeException(s"Unexpected output: ${pt.stdout}")
      }
    }
  }

  // Given the result of an ltsinfo invocation (via util.runCommand),
  // parse the process output and return the number of states
  private def ltsinfoResult(pt: util.StdOutErrAndTime): Long = {
    val res = pt.stderr.split("\n")(0)
    val nstatesRegex = raw"Number of states: (\d+)\.$$".r
    res match {
      case nstatesRegex(nstates) => nstates.toLong
    }
  }

  // Remove the given directory with all its files and sub-directories
  private def deleteDir(d: java.io.File): Unit = {
    assert(d.isDirectory && d.canWrite)

    val dirs = d.listFiles.filter(_.isDirectory)
    for { dir <- dirs } deleteDir(dir)

    val files = d.listFiles.filter(_.isFile)
    assert(files.forall(_.canWrite))

    val delresult = for { f <- files } yield f.delete()
    assert(delresult.forall(x => x))

    d.delete()
  }

  override val toString: String = s"Verifier(${spec.description})"
}

object Verifier {
  /** Create a verifier that checks whether {@code spec} satisfies
    * the given {@code property}.
    */
  def apply(spec: Spec, property: Property): Verifier = {
    new Verifier(spec, Set(property))
  }

  /** Create a verifier that checks whether {@code spec} satisfies
    * the given {@code properties}.
    */
  def apply(spec: Spec, properties: Set[Property]): Verifier = {
    new Verifier(spec, properties)
  }
}
