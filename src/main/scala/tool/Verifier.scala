// mpstk - the Multiparty Session Types toolKit
// Copyright 2018 Alceste Scalas <alceste.scalas @ imperial.ac.uk>
// Released under the MIT License: https://opensource.org/licenses/MIT
package mpstk.tool

import mpstk._

import scala.collection.immutable.ListMap

import java.nio.file.{Path, Paths}

import picocli.{CommandLine => cli}

/** Benchmark type */
protected[tool] object BenchmarkType extends Enumeration {
  type Type = Value
  val None, RealTime, Default, EndToEnd = Value
}


/** Implementation of the command line verification tool. */
object Verifier extends Common {
  override val TOOL_NAME = "mpstk verify"

  /** Tool configuration, set up via command line options */
  @cli.Command(name = "mpstk verify",
               version = Array("0.1"), // FIXME: synchronise with sbt
               description = Array("Verify global session types and multiparty session typing contexts"))
  private class Config extends CommonConfig {
    final val OPTION_BENCHMARK = "--benchmark"
    final val OPTION_BENCHTYPE = "--benchmark-type"
    final val OPTION_LIST_PROPERTIES = "--list-properties"
    final val OPTION_KEEPTEMP = "--keep-temp"

    @cli.Spec
    var spec: cli.Model.CommandSpec = null // Injected by picocli

    @cli.Option(names = Array("-b", OPTION_BENCHMARK),
                paramLabel = "N",
                description = Array("benchmark with <N> repetitions (default: N=0, no benchmark)"))
    var benchmark: Int = 0 // Number of benchmark repetitions

    var benchmarkType: BenchmarkType.Type = BenchmarkType.None
    @cli.Option(names = Array("-t", OPTION_BENCHTYPE),
                paramLabel = "T",
                description = Array(
                  "what is timed in a benchmark. <T> can be one of:",
                  "  default  - call 'pbes2bool' and get the result",
                  "  realtime - 'time pbes2bool' (no JVM overhead)",
                  "  endtoend - from file loading to verification"))
    def setBenchmarkType(tpe: String): Unit = tpe match {
      case "default"  => benchmarkType = BenchmarkType.Default
      case "realtime" => benchmarkType = BenchmarkType.RealTime
      case "endtoend"    => benchmarkType = BenchmarkType.EndToEnd
      case _         => {
        throw new cli.ParameterException(spec.commandLine(),
                                         s"Invalid benchmark type: ${tpe}")
      }
    }

    @cli.Option(names = Array("-o", "--output-csv"),
                paramLabel = "CSVFILE",
                description = Array("write CSV output on <CSVFILE>"))
    var csvOutFile: Path = null // Output file for results

    @cli.Option(names = Array("-k", OPTION_KEEPTEMP),
                description = Array("keep temporary files (default: false)"))
    var keepTemp: Boolean = false


    @cli.Option(names = Array("-l", OPTION_LIST_PROPERTIES),
                description = Array("print the list of available properties"))
    var listProperties: Boolean = false

    var properties: Array[mcrl2.Property] = mcrl2.Properties.all.toArray
    @cli.Option(names = Array("-p", "--properties"),
                paramLabel = "PROPS",
                description = Array("comma-separated list of properties to be verified (default: all)"))
    def setProperties(props: String): Unit = {
      properties = props.split(",").map { p =>
        val props = mcrl2.Properties.all.filter { _.filename == p }
        if (props.size == 0) {
          throw new cli.ParameterException(spec.commandLine(),
                                           s"Invalid property: ${p}")
        }
        props.toSeq(0)
      }
    }
 
    @cli.Option(names = Array("-s", "--count-states"),
                description = Array("print the number of states (default: false)"))
    var countStates: Boolean = false

    @cli.Parameters(arity = "0..*", paramLabel = "FILE",
                    description = Array("file(s) to process (extensions: .global, .ctx)"))
    var paths: Array[Path] = Array()
  }

  /** Main entry point. */
  def main(args: Array[String]): Unit = {
    val cfg = new Config
    val cmdline = new picocli.CommandLine(cfg)
    try {
      cmdline.parse(args:_*)

      setLogLevel(cfg)

      if (cfg.printHelp) {
        if (cfg.paths.length != 0) {
          printError( s"""Cannot process files with "${cfg.OPTION_HELP}"""")
        }
        printHelp(cmdline)
      }

      if (cfg.listProperties) {
        if (cfg.paths.length != 0) {
          printError(s"""Cannot process files with "${cfg.OPTION_LIST_PROPERTIES}"""")
        }
        listProperties()
      }

      if (cfg.benchmark < 0) {
        printError("The number of benchmark repetitions cannot be negative")
      }

      if ((cfg.benchmark == 0) && cfg.benchmarkType != BenchmarkType.None) {
        printError(s"Option ${cfg.OPTION_BENCHTYPE} can only be used with ${cfg.OPTION_BENCHMARK}")
      }

      if (cfg.keepTemp && cfg.benchmarkType == BenchmarkType.EndToEnd) {
        printError(s"Option ${cfg.OPTION_KEEPTEMP} cannot be used with end-to-end benchmark")
      }

      if (cfg.paths.length == 0) {
        printError("Missing required parameter: FILE")
      }

      // Validate all filename arguments
      cfg.paths.foreach { p =>
        if (!(p.toString.endsWith(FILE_EXT_GLOBAL_TYPE))
              && !(p.toString.endsWith(FILE_EXT_TYPING_CONTEXT))) {
            printError(s"${p}: unsupported file extension")
        }
        checkReadableFile(p)
      }

      val (results, verifiersMap): (Map[Path, Map[mcrl2.Property,String]],
                                    Map[Path, mcrl2.Verifier]) = {
        if (cfg.benchmarkType == BenchmarkType.EndToEnd) {
          // Trick: we change the configuration to let computerResult
          // perform a normal (non-benchmarking) execution; then, we
          // collect the results
          val benchReps = cfg.benchmark
          val benchCountStates = cfg.countStates

          cfg.benchmark = 0
          cfg.benchmarkType = BenchmarkType.None
          cfg.countStates = false
          assert(!cfg.keepTemp)

          val benchPaths = cfg.paths
          val benchProps = cfg.properties

          val results = Map(benchPaths.map { path =>
            // Only benchmark one file a time
            cfg.paths = Array(path)
            (path, Map(benchProps.map { prop =>
              // Only benchmark one property a time
              cfg.properties = Array(prop)
              val durations = (1 to benchReps).map { _ =>
                val startTime = System.nanoTime()
                val res = computeResults(cfg)
                val endTime = System.nanoTime()

                // Don't forget to get cleanup all verifiers' resources
                res._2.values.foreach { verifier => verifier.close() }

                nanosToSecs(endTime - startTime)
              }
              (prop, meanStdDevPerc(durations))
            }:_*))
          }:_*)

          // Restore the previous configuration
          cfg.paths = benchPaths
          cfg.properties = benchProps
          cfg.countStates = benchCountStates

          // FIXME: can we avoid building and returning this dummy thing?
          val verifiersMap = Map(cfg.paths.map { p =>
            val spec: mcrl2.Spec = buildSpec(p)
            (p, mcrl2.Verifier(spec, cfg.properties.toSet))
          }:_*)
          (results, verifiersMap) 
        } else {
          computeResults(cfg)
        }
      }
        
      printTable(cfg.paths, verifiersMap, results, cfg)
  
      if (!(cfg.csvOutFile == null)) {
        writeCSV(cfg.paths, verifiersMap, results, cfg)
      }
  
      if (cfg.keepTemp) {
        println("Temporary directories:")
        cfg.paths.foreach { p =>
          println(s"  * ${p} => ${verifiersMap(p).tempDir}")
        }
      } else {
        verifiersMap.values.foreach { v => v.close() }
      }
    } catch {
      case e: IllegalArgumentException => {
        printError(e.getMessage)
      }
      case e: cli.ParameterException => {
        printError(e.getMessage)
      }
    }
  }

  /** Compute the verification or benchmark results */
  private
  def computeResults(cfg: Config): (Map[Path, Map[mcrl2.Property,String]],
                                    Map[Path, mcrl2.Verifier]) = {
    // Here we use ListMap to preserve key order from filePaths
    val specs: Map[Path, mcrl2.Spec] = ListMap(cfg.paths.map { p =>
      (p, buildSpec(p))
    }.toSeq:_*)

    val verifiersMap: Map[Path, mcrl2.Verifier] = Map(specs.map { (pspec) =>
      logger.debug(s"Creating verifier for ${pspec._2}")
      (pspec._1, mcrl2.Verifier(pspec._2, cfg.properties.toSet))
    }.toSeq:_*)

    if (cfg.benchmark >= 1) {
      (benchmark(verifiersMap, cfg), verifiersMap)
    } else {
      (verify(verifiersMap, cfg), verifiersMap)
    }
  }

  /** Try to build an mCRL2 specification from the given path */
  private def buildSpec(p: Path): mcrl2.Spec = {
    val fname = p.toString
    if (fname.endsWith(FILE_EXT_GLOBAL_TYPE)) {
      parser.GlobalTypeParser.parse(p) match {
        case parser.GlobalTypeParser.NoSuccess(msg, input) => {
          printParseError(fname, input.pos, msg)
          throw new RuntimeException("Unreachable")
        }
        case parser.GlobalTypeParser.Success(gt, _) => {
          mcrl2.Spec(gt, fname) match {
            case Right(spec) => spec
            case Left(msg) => {
              printError(s"${fname}: unprojectable global type:\n${msg}",
                         false)
              throw new RuntimeException("Unreachable")
            }
          }
        }
      }
    } else if (fname.endsWith(FILE_EXT_TYPING_CONTEXT)) {
      parser.ContextParser.parse(p) match {
        case parser.ContextParser.NoSuccess(msg, input) => {
          printParseError(fname, input.pos, msg)
          throw new RuntimeException("Unreachable")
        }
        case parser.ContextParser.Success(ctx, _) => {
          mcrl2.Spec(ctx, fname)
        }
      }
    } else {
      throw new RuntimeException(s"BUG: invalid file extension: ${fname}")
    }
  }

  /** Print the list of available verification properties, and exit with
    * code 0 */
  private def listProperties(): Unit = {
    println("Available verification properties:")
    mcrl2.Properties.all.foreach { p => println(s" * ${p.filename}") }
    System.exit(0)
  }

  /** Perform a benchmark on the given verifiers */
  private
  def benchmark(verifMap: Map[Path, mcrl2.Verifier],
                cfg: Config): Map[Path, Map[mcrl2.Property, String]] = {
    assert(cfg.benchmark != 0)
    val reps = cfg.benchmark
    assert(reps > 0)

    verifMap.map { pverif =>
      val realTime: Boolean = cfg.benchmarkType == BenchmarkType.RealTime
      (pverif._1,
       pverif._2.benchmark(reps, realTime) map { pres =>
         assert(pres._2.length == reps)
         val resD = pres._2.map { x => nanosToSecs(x) }
         (pres._1, meanStdDevPerc(resD))
       })
    }
  }

  /** Convert nanoseconds to seconds, in double precision floating point */
  private def nanosToSecs(x: Long): Double = {
    (x / 100000).toDouble / 10000.0
  }

  /** Compute the mean of a given sequence of values */
  private def mean(xs: Seq[Double]): Double = {
    xs.sum / xs.length
  }

  /** Compute the standard deviation of a sequence of values */
  private def stdDev(xs: Seq[Double]): Double = {
    val mn = mean(xs)
    Math.sqrt(xs.map { x =>
                val diff = x - mn
                diff * diff
              }.sum / xs.length)
  }

  /** String with the mean ± stddev (in percentage) of a sequence of values */
  private def meanStdDevPerc(xs: Seq[Double]): String = {
    val mn = mean(xs)
    val stdd = stdDev(xs)
    val stdDevPerc = ((stdd / mn) * 100.0).toInt
    f"${mn}%1.2f ± ${stdDevPerc}%%"
  }

  /** Retrieve the verification results of the given verifiers */
  private
  def verify(verifMap: Map[Path, mcrl2.Verifier],
             cfg: Config): Map[Path, Map[mcrl2.Property, String]] = {
    assert(cfg.benchmark == 0)
    verifMap.map { pverif =>
      (pverif._1, pverif._2.results.map { pres =>
         (pres._1, pres._2.toString) })
    }
  }

  /** Save test results in a CSV file.
    *
    * @param addStates add a column with the number of states in each verifier
    */
  private
  def writeCSV(paths: Seq[Path],
               verifMap: Map[Path, mcrl2.Verifier],
               resultsMap: Map[Path, Map[mcrl2.Property, String]],
               cfg: Config): Unit = {
    import com.github.tototoshi.csv.CSVWriter

    assertVerification(paths, verifMap, resultsMap, cfg)
    assert(cfg.csvOutFile != null)

    val outfile = cfg.csvOutFile.toFile

    logger.debug(s"Writing CSV output to ${outfile}")
    val csvw = CSVWriter.open(outfile)

    // Write headers
    val headers = (List("protocol") ++
                     (if (cfg.countStates) List("states") else List()) ++
                     cfg.properties.map(_.filename))
    logger.debug(s"""CSV headers: ${headers.mkString(",")}""")
    csvw.writeRow(headers)

    // Write rows
    paths.foreach { p =>
      val row = (List(p.toString) ++
                 (if (cfg.countStates) List(s"${verifMap(p).states}")
                  else List()) ++
                   cfg.properties.map { prop => resultsMap(p)(prop) })
      logger.debug(s"""Writing CSV row: ${row.mkString(",")}""")
      assert(row.length == headers.length)
      csvw.writeRow(row)
    }

    csvw.close()
    logger.debug(f"CSV output written on ${outfile}")
  }

  /** Pretty print the output data in a table */
  private
  def printTable(paths: Seq[Path],
                 verifMap: Map[Path, mcrl2.Verifier],
                 resultsMap: Map[Path, Map[mcrl2.Property, String]],
                 cfg: Config): Unit = {
    assertVerification(paths, verifMap, resultsMap, cfg)

    // FIXME: duplicated code from writeCSV above
    val headers = (List("protocol") ++
                     (if (cfg.countStates) List("states") else List()) ++
                     cfg.properties.map(_.shortName))
    val rows: Seq[Seq[String]] = paths.map { p =>
      List(p.toString) ++
      (if (cfg.countStates) List(s"${verifMap(p).states}") else List()) ++
      cfg.properties.map { prop => resultsMap(p)(prop) }
    }

    val table = Seq(headers) ++ rows

    val widths: Seq[Seq[Int]] = (
      table.map { row => row.map { f => f.size } }
    )

    val lastColIdx = headers.length - 1
    val colWidths = (0 to lastColIdx).map { col =>
      widths.map { row =>
        row(col)
      }.max
    }

    val out: String = table.foldLeft("") { (rowsAcc, row) =>
      val rowStr = (0 to lastColIdx).foldLeft("") { (colAcc, col) =>
        val field = row(col)
        val last = (lastColIdx == col)
        val padding = (colWidths(col) - field.size) + (if (!last) 1 else -1)
        colAcc ++ field ++
          (0 to padding).foldLeft("") { (acc, _) => acc ++ " " }
      }
      rowsAcc ++ rowStr ++ "\n"
    }

    println(out ++ "\nLegend:")
    cfg.properties.foreach {
      p => println(s" * ${p.shortName}: ${p.filename}")
    }
  }

  /** Perform a series of consistency checks on verification outcomes */
  private
  def assertVerification(paths: Seq[Path],
                         verifMap: Map[Path, mcrl2.Verifier],
                         resultsMap: Map[Path, Map[mcrl2.Property, String]],
                         cfg: Config): Unit = {
    assert(paths.length > 0)
    assert(verifMap.keySet == paths.toSet)
    assert(verifMap.keySet == resultsMap.keySet)
    assert(resultsMap(paths(0)).keySet.size == cfg.properties.length)
  }
}
