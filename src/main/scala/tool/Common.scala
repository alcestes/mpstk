// mpstk - the Multiparty Session Types toolKit
// Copyright 2018 Alceste Scalas <alceste.scalas @ imperial.ac.uk>
// Released under the MIT License: https://opensource.org/licenses/MIT
package mpstk.tool

import java.nio.file.Path

import com.typesafe.scalalogging.LazyLogging

import picocli.{CommandLine => cli}

/** Common base class for all tools */
protected[tool] abstract class Common extends LazyLogging {
  val TOOL_NAME: String
  val FILE_EXT_GLOBAL_TYPE = ".global"
  val FILE_EXT_TYPING_CONTEXT = ".ctx"

  /** Common class for configurations set up via command line options */
  protected[tool] class CommonConfig {
    final val OPTION_HELP = "--help"

    @cli.Option(names = Array("-d", "--debug"),
                description = Array("print debugging information (default: false)"))
    var debug: Boolean = false

    @cli.Option(names = Array("-h", OPTION_HELP),
                usageHelp = true,
                description = Array("print this usage information"))
    var printHelp: Boolean = false
  }

  /** Configure the log level. */
  def setLogLevel(cfg: CommonConfig): Unit = {
    import ch.qos.logback.classic.{Level,Logger}
    import org.slf4j.LoggerFactory
    import org.slf4j.Logger.ROOT_LOGGER_NAME

    val logger = LoggerFactory.getLogger(ROOT_LOGGER_NAME).asInstanceOf[Logger]
    if (cfg.debug) {
      logger.setLevel(Level.DEBUG)
    } else {
      logger.setLevel(Level.WARN)
    }
  }

  /** Check whether the given path points to a readable regular file,
    * and exit with an error if not */
  def checkReadableFile(p: Path): Unit = {
    val file = p.toFile
    if (!file.exists) {
      printError(s"${p}: not found")
    } else if (!file.isFile) {
      printError(s"${p}: not a file")
    } else if (!file.canRead) {
      printError(s"${p}: not readable")
    }
  }

  /** Print a parsing error, and exit with code 1 */
  def printParseError(filename: String,
                      pos: scala.util.parsing.input.Position,
                      msg: String): Unit = {
    System.err.println(s"${TOOL_NAME}: ${filename}: "
        ++ s"error on line ${pos.line}, column ${pos.column}:\n"
        ++ s"${pos.longString}\n${msg}")
    System.exit(1)
  }

  /** Print an error with minimal usage information, and exit with code 1 */
  def printError(err: String, printUsage: Boolean = true): Unit = {
    System.err.println(s"${TOOL_NAME}: ${err}")
    if (printUsage) {
      System.err.println(s"Usage: ${TOOL_NAME} [OPTION...] [FILE...]")
      System.err.println(s"Try '${TOOL_NAME} --help' for more information.")
    }
    System.exit(1)
  }

  /** Print usage information, and exit with code 0 */
  def printHelp(cmdline: picocli.CommandLine): Unit = {
    cmdline.usage(System.out);
    System.exit(0)
  }
}
