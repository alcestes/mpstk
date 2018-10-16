// mpstk - the Multiparty Session Types toolKit
// Copyright 2018 Alceste Scalas <alceste.scalas @ imperial.ac.uk>
// Released under the MIT License: https://opensource.org/licenses/MIT
package mpstk.tool

import mpstk._

import scala.collection.immutable.ListMap

import java.nio.file.{Path, Paths}

import picocli.{CommandLine => cli}

/** Implementation of the command line projection tool. */
object Projector extends Common {
  override val TOOL_NAME = "mpstk project"

  /** Tool configuration, set up via command line options */
  @cli.Command(name = "mpstk project",
               version = Array("0.1"), // FIXME: synchronise with sbt
               description = Array("Project global session types"))
  private class Config extends CommonConfig {
    @cli.Spec
    var spec: cli.Model.CommandSpec = null // Injected by picocli

    var roles: Array[Role] = Array()
    @cli.Option(names = Array("-r", "--roles"),
                paramLabel = "ROLES",
                description = Array("comma-separated list of roles to project (default: all roles in the global type)"))
    def setRoles(rolesStr: String): Unit = {
      roles = rolesStr.split(",").map { r => Role(r) }
    }

    var session: Session = Session("s")
    @cli.Option(names = Array("-s", "--session"),
                paramLabel = "SESSION",
                description = Array("name of the session for the projected typing context (default: s)"))
    def setSession(s: String): Unit = {
      session = Session(s)
    }

    var path: Option[Path] = None
    @cli.Parameters(arity = "0..1", paramLabel = "FILE",
                    description = Array("file to project (extension: .global)"))
    def setPath(p: Path): Unit = {
      if (!p.toString.endsWith(FILE_EXT_GLOBAL_TYPE)) {
        printError(s"${p}: unsupported file extension")
      }
      checkReadableFile(p)      
      path = Some(p)
    }
  }

  /** Main entry point. */
  def main(args: Array[String]): Unit = {
    val cfg = new Config
    val cmdline = new picocli.CommandLine(cfg)
    try {
      cmdline.parse(args:_*)

      setLogLevel(cfg)

      if (cfg.printHelp) {
        if (!cfg.path.isEmpty) printError(
          s"""Cannot process files with "${cfg.OPTION_HELP}"""")
        printHelp(cmdline)
      }

      if (cfg.path.isEmpty) {
        printError("Missing required parameter: FILE")
      }

      val path = cfg.path.get

      parser.GlobalTypeParser.parse(path) match {
        case parser.GlobalTypeParser.NoSuccess(msg, input) => {
          printParseError(path.toString, input.pos, msg)
          throw new RuntimeException("Unreachable")
        }
        case parser.GlobalTypeParser.Success(gt, _) => {
          if (cfg.roles.size == 0) {
            // Use all roles in the global type
            cfg.roles = gt.roles.toArray
          }
          val projs = Map(cfg.roles.map(r => (r, gt.projection(r))):_*)
          
          if (projs.values.exists(_.isLeft)) {
            val err = "Undefined projections:\n" ++ (
              projs.foldLeft(List[String]()) {
                case (acc, (r, Left(err))) => acc :+ s" * ${r}: ${err}"
                case (acc, (_, Right(_))) => acc
              }.mkString("\n"))
            printError(err, false)
          } else {
            val res = cfg.roles.foldLeft(List[String]()) { (acc, r) =>
              acc ++ List(s"${Channel(cfg.session, r)}: ${projs(r).right.get}")
            }.mkString(",\n")
            println(res)
          }
        }
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
}
