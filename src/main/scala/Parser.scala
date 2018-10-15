package mpstk.parser

import scala.util.parsing.combinator._
import scala.language.postfixOps

import mpstk.{Channel, Label, Role, Session, Type, GroundType}
import mpstk.{GlobalType, MPST, BasePayloadCont, PayloadCont,
              Branch, Select, Rec, RecVar, End}
import mpstk.Context
import mpstk.raw

protected[parser] case class ParserConfig[A, PC <: BasePayloadCont[A]](
  PayloadCont: (Type, A) => PC,
  endPayload: Type,
  endCont: A
)

abstract trait BaseParser extends RegexParsers {
  def comment: Parser[String] = """(?m)#.*$""".r
  def comments: Parser[Unit] = rep(comment) ^^ { _ => () }

  def identifier: Parser[String] = """[a-zA-Z]\w*""".r

  def label: Parser[Label] = identifier ^^ { l => Label(l) }
  def role:  Parser[Role]  = identifier ^^ { r => Role(r) }

  def ground: Parser[GroundType] = bool | int | string | unit

  def bool: Parser[GroundType.Bool.type] = "[Bb]ool".r ^^ {
    _ => GroundType.Bool
  }
  def int: Parser[GroundType.Int.type] = "[Ii]nt".r ^^ {
    _ => GroundType.Int
  }
  def string: Parser[GroundType.String.type] = {
    ("[Ss]tring".r | "[Ss]tr".r) ^^ { _ => GroundType.String }
  }
  def unit: Parser[GroundType.Unit.type] = "[Uu]nit".r ^^ {
    _ => GroundType.Unit
  }

  def choice[A, PC <: BasePayloadCont[A]](tpe: Parser[Type],
                             cont: Parser[A],
                             cfg: ParserConfig[A, PC]): Parser[(Label, PC)] = {
    label ~ payloadcont(tpe, cont, cfg) ^^ { lpc =>
      (lpc._1, lpc._2)
    }
  }

  def payloadcont[A, PC <: BasePayloadCont[A]](tpe: Parser[Type],
                                      cont: Parser[A],
                                      cfg: ParserConfig[A, PC]): Parser[PC] = {
    payloadcontFull(tpe, cont, cfg) |
    payloadcontNoPay(cont, cfg) |
    payloadcontNoCont(tpe, cfg) |
    payloadcontEmpty(cfg)
  }

  def payloadcontFull[A, PC <: BasePayloadCont[A]](tpe: Parser[Type],
                                      cont: Parser[A],
                                      cfg: ParserConfig[A, PC]): Parser[PC] = {
    ("(" ~> tpe <~ ")") ~ ("." ~> cont) ^^ {
      pc => cfg.PayloadCont(pc._1, pc._2)
    }
  }

  def payloadcontNoPay[A, PC <: BasePayloadCont[A]](cont: Parser[A],
                                      cfg: ParserConfig[A, PC]): Parser[PC] = {
    (("(" ~ ")")?) ~> ("." ~> cont) ^^ { cnt =>
      cfg.PayloadCont(cfg.endPayload, cnt)
    }
  }

  def payloadcontNoCont[A, PC <: BasePayloadCont[A]](tpe: Parser[Type],
                                      cfg: ParserConfig[A, PC]): Parser[PC] = {
    ("(" ~> tpe <~ ")") ^^ { pay =>
      cfg.PayloadCont(pay, cfg.endCont)
    }
  }

  def payloadcontEmpty[A, PC <: BasePayloadCont[A]](cfg: ParserConfig[A, PC]): Parser[PC] = {
    "" ^^ { _ => cfg.PayloadCont(cfg.endPayload, cfg.endCont) }
  }

  def choices[A, PC <: BasePayloadCont[A]](tpe: Parser[Type],
                       cont: Parser[A],
                       cfg: ParserConfig[A, PC]): Parser[List[(Label, PC)]] = {
    choicesMulti(tpe, cont, cfg) | choicesSingle(tpe, cont, cfg)
  }

  def choicesMulti[A, PC <: BasePayloadCont[A]](tpe: Parser[Type],
                       cont: Parser[A],
                       cfg: ParserConfig[A, PC]): Parser[List[(Label, PC)]] = {
    "{" ~> rep1sep(choice(tpe, cont, cfg), ",") <~ "}" ^? ({ 
      case l: List[(Label, PC)] if (
        // Ensure that labels are unique
        l.map(_._1).distinct.size == l.size
      ) => l
    }, { l =>
      val labels = l.map { _._1 }
      val dupl = labels.filter { l =>
        labels.indexOf(l) != labels.lastIndexOf(l)
      }.distinct
      f"""Choice with duplicated label(s): ${dupl.mkString(", ")}"""
    })
  }

  def choicesSingle[A, PC <: BasePayloadCont[A]](tpe: Parser[Type],
                       cont: Parser[A],
                       cfg: ParserConfig[A, PC]): Parser[List[(Label, PC)]] = {
    choice(tpe, cont, cfg) ^^ { c => List(c) }
  }
}

protected[parser]
class MPSTParser extends BaseParser {
  private val cfg = ParserConfig[MPST, PayloadCont](
    (payload: Type, cont: MPST) => PayloadCont(payload, cont),
    End,
    End
  )

  def tpe: Parser[Type] = ground | mpst

  // NOTE: always try to match recvar last (otw, it might capture e.g. "end")
  def mpst: Parser[MPST] = {
    ("(" ~> mpst <~ ")") | branch | select | end | rec | recvar
  }

  def closedmpst: Parser[MPST] = mpst ^? ({
    case t: MPST if t.closed => t
  }, { t =>
    f"The session must be closed (free variable(s): ${t.fv.mkString(", ")})"
  })

  def end: Parser[End.type] = "end".r ^^ { _ => End }

  def branchSym: Parser[String] = "&"
  def branch: Parser[Branch] = {
    role ~ (branchSym ~> choices(tpe, mpst, cfg)) ^^ { rc =>
      Branch(rc._1, Map(rc._2:_*))
    }
  }

  def selectSym: Parser[String] = "⊕" | "(+)"
  def select: Parser[Select] = {
    role ~ (selectSym ~> choices(tpe, mpst, cfg)) ^^ { rc =>
      Select(rc._1, Map(rc._2:_*))
    }
  }

  def recSym: Parser[String] = "μ" | "rec"
  def rec: Parser[Rec] = (((recSym ~ "(") ~> recvar <~ ")") ~ mpst) ^^ { rm =>
    Rec(rm._1, rm._2)
  } ^? ({
    case t: Rec if t.guarded => t
  }, { t =>
    s"Unguarded recursion on ${t.recvar}"
  })

  def recvar: Parser[RecVar] = identifier ^^ { name => RecVar(name) }
}

/** Session type parser. */
object MPSTParser extends MPSTParser {
  /** Parse a session type from a string. */
  def parse(input: String): ParseResult[MPST] = {
    parseAll(comments ~> mpst, input)
  }
}

protected[parser]
class ContextParser extends MPSTParser {
  def session: Parser[Session] = identifier ^^ { s => Session(s) }

  def channel: Parser[Channel] = session ~ ("[" ~> role <~ "]") ^^ {
    sr => Channel(sr._1, sr._2)
  }

  def entry: Parser[(Channel, MPST)] = channel ~ (":" ~> closedmpst) ^^ { cs =>
    (cs._1, cs._2)
  }

  def entries: Parser[List[(Channel, MPST)]] = {
    repsep(entry, ",") ^? ({
      case l: List[(Channel, MPST)] if (
        // Ensure that channels are unique
        l.map(_._1).distinct.size == l.size
      ) => l
    }, { l =>
      val channels = l.map { _._1 }
      val dupl = channels.filter { c =>
        channels.indexOf(c) != channels.lastIndexOf(c)
      }.distinct
      f"""Context with duplicated channel(s): ${dupl.mkString(", ")}"""
    })
  }

  def context: Parser[Context] = entries ^^ { entries => Context(entries:_*) }
}

/** Parser for session typing contexts. */
object ContextParser extends ContextParser {
  import java.nio.file.{Files, Path, Paths}
  import scala.collection.JavaConverters._

  /** Parse a session typing context from a string. */
  def parse(input: String): ParseResult[Context] = {
    parseAll(comments ~> context, input)
  }

  /** Parse a session typing context from a file, given as {@code Path}.
    * 
    * @throws java.io.IOException in case of I/O error
    */
  def parse(input: Path): ParseResult[Context] = {
    parse(Files.readAllLines(input).asScala.mkString("\n"))
  }

  /** Parse a session typing context from a file, given as {@code String}.
    * 
    * @throws java.io.IOException in case of I/O error
    * @throws java.nio.file.InvalidPathException if {@code filename} is invalid
    */
  def parseFile(filename: String): ParseResult[Context] = {
    parse(Paths.get(filename))
  }
}

protected[parser]
class GlobalTypeParser extends MPSTParser {
  private val cfg = ParserConfig[GlobalType, GlobalType.PayloadCont](
    (payload: Type, cont: GlobalType) => GlobalType.PayloadCont(payload, cont),
    End,
    GlobalType.End
  )

  private def payloadCont(payload: Type, cont: GlobalType) = {
    GlobalType.PayloadCont(payload, cont)
  }
  private val endPayload = End
  private val endCont = GlobalType.End

  // NOTE: always try to match gtrecvar last (otw, it might capture e.g. "end")
  def globaltype: Parser[GlobalType] = {
    ("(" ~> globaltype <~ ")") | comm | gtend | gtrec | gtrecvar
  }

  // We only accept closed types
  override def tpe: Parser[Type] = ground | closedmpst

  def gtend: Parser[GlobalType.End.type] = "end".r ^^ { _ => GlobalType.End }

  def commSym: Parser[String] = "→" | "->"
  def comm: Parser[GlobalType.Comm] = {
    role ~ (commSym ~> role <~ (":"?)) ~ choices(tpe, globaltype, cfg) ^^ { rc=>
      GlobalType.Comm(rc._1._1, rc._1._2, Map(rc._2:_*))
    }
  }

  // TODO: refactor the following, to avoid code duplication?
  def gtrecSym: Parser[String] = "μ" | "rec"
  def gtrec: Parser[GlobalType.Rec] = {
    (((gtrecSym ~ "(") ~> gtrecvar <~ ")") ~ globaltype) ^^ { rm =>
      GlobalType.Rec(rm._1, rm._2)
    }
  } ^? ({
    case t: GlobalType.Rec if t.guarded => t
  }, { t =>
    s"Unguarded recursion on ${t.recvar}"
  })

  def gtrecvar: Parser[GlobalType.RecVar] = identifier ^^ {
    name => GlobalType.RecVar(name)
  }
}

/** Parser for global types. */
object GlobalTypeParser extends GlobalTypeParser {
  import java.nio.file.{Files, Path, Paths}
  import scala.collection.JavaConverters._

  /** Parse a global type from a string. */
  def parse(input: String): ParseResult[GlobalType] = {
    parseAll(comments ~> globaltype, input)
  }

  /** Parse a global type from a file, given as {@code Path}.
    * 
    * @throws java.io.IOException in case of I/O error
    */
  def parse(input: Path): ParseResult[GlobalType] = {
    parse(Files.readAllLines(input).asScala.mkString("\n"))
  }

  /** Parse a global type from a file, given as {@code String}.
    * 
    * @throws java.io.IOException in case of I/O error
    * @throws java.nio.file.InvalidPathException if {@code filename} is invalid
    */
  def parseFile(filename: String): ParseResult[GlobalType] = {
    parse(Paths.get(filename))
  }
}
