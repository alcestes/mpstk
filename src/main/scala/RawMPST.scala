// mpstk - the Multiparty Session Types toolKit
// Copyright 2018 Alceste Scalas <alceste.scalas @ imperial.ac.uk>
// Released under the MIT License: https://opensource.org/licenses/MIT
package mpstk.raw

import mpstk.{Channel, Label, Role, Session, Type, GroundType, ContextImpl}

// "Raw" multiparty session type, used internally
protected[mpstk] sealed abstract class MPST {
  /** For all types in the given {@code session}, add an input branch to
    * all branching subterms that receive from {@code from} a message
    * label {@code label}, with a payload type that is a strict supertype
    * of {@code payload}.
    */
  def addSubBranches(from: Role, label: Label, payload: Type) = {
    ops.addSubBranches(this, from, label, payload)
  }

  def toMPST: Option[mpstk.MPST] = ops.rawToMPST(this)
}

protected[mpstk] case object End extends MPST {
  override def toString = "end"
}

// Payload + continuation
protected[mpstk] case class PayloadCont(payload: Type, cont: MPST) {
  override def toString = s"(${payload}).${cont}"
}

// A generic choice, abstracting branching and selection types.
// Note that, unlike mpstk.Choice, here labels can be duplicated, provided
// that they are paired with distinct continuations
protected[mpstk]
sealed abstract class Choice(val choices: Set[(Label, PayloadCont)]) extends MPST {
  override def toString = {
    val cs = choices.map { lpc => s"${lpc._1}${lpc._2}" }
    cs.mkString(", ")
  }
}

protected[mpstk]
case class Branch(from: Role,
                  override val choices: Set[(Label, PayloadCont)]) extends Choice(choices) {
  override def toString = s"${from}&{${super.toString}}"
}

protected[mpstk]
case class Select(to: Role,
                  override val choices: Set[(Label, PayloadCont)]) extends Choice(choices) {
  override def toString = s"${to}⊕{${super.toString}}"
}

protected[mpstk] case class Rec(recvar: RecVar, body: MPST) extends MPST {
  override def toString = s"μ(${recvar})${body}"
}

protected[mpstk] case class RecVar(name: String) extends MPST {
  override def toString = s"${name}"
}

protected[mpstk]
class Context(orig: mpstk.Context,
              impl: Map[Channel, MPST]) extends ContextImpl[MPST](impl) {
  /** For all types in the given {@code session}, add an input branch to
    * all branching subterms that receive from {@code from} a message
    * label {@code label}, with a payload type that is a strict supertype
    * of {@code payload}.
    */
  def addSubBranches(s: Session, from: Role, to: Role,
                     label: Label, payload: Type): Context = {
    val chan = Channel(s, to)
    this.get(chan) match {
      case Some(t) => {
        Context(orig,
          (this + (chan -> t.addSubBranches(from, label, payload)))
        )
      }
      case None => this
    }
  }

  /** Apply {@code addSubBranches} to all entries in the typing context */
  def addSubBranches: Context = {
    val channels = this.keySet
    val sessions = channels.map(_.session).toSet

    // A potential output
    case class Output(from: Role, to: Role, label: Label, payload: Type)
    val outputs: Map[Session, Seq[Output]] = Map({
      for {
        s <- sessions
      } yield (s, (for {
                     c <- channels.filter(_.session == s)
                     to <- channels.filter { ch =>
                       c.session == s && ch.role != c.role
                     } map { _.role }
                     out <- orig(c).outputs(to)
                   } yield Output(c.role, to, out._1, out._2)).toSeq)
    }.toSeq:_*)

    outputs.foldLeft(this) { (acc, so) =>
      so._2.foldLeft(acc) { (acc2, o) =>
        acc2.addSubBranches(so._1, o.from, o.to, o.label, o.payload)
      }
    }
  }
}

object Context {
  def apply(orig: mpstk.Context): Context = {
    new Context(orig, Map(orig.toSeq.map(ct => (ct._1, ct._2.toRaw)):_*))
  }

  protected[raw]
  def apply(orig: mpstk.Context, impl: Map[Channel, MPST]): Context = {
    new Context(orig, impl)
  }
}

protected[mpstk] object ops {
  def valid(t: MPST): Boolean = t match {
    case End => true
    case c: Choice => {
      !(c.choices.isEmpty) && c.choices.forall { (lpc) => valid(lpc._2.cont) }
    }
    case Rec(_, body) => valid(body)
    case RecVar(_) => true
  }

  // Add an input branch to all branching subterms that receive from @from
  // a label @label, with a payload type that is a strict supertype of @payload
  protected[raw]
  def addSubBranches(t: MPST, from: Role, label: Label,
                     payload: Type): MPST = t match {
    case End => End
    case Branch(f, choices) if (f == from) => {
      val choices2 = choices.filter { lpc =>
        ((lpc._1 == label) && (payload != lpc._2.payload) &&
         (payload :< lpc._2.payload))
      } map { lpc =>
        (lpc._1, PayloadCont(payload, lpc._2.cont))
      }
      val newChoices = choices.union(choices2)
      Branch(from, newChoices.map { lpc =>
        (lpc._1, PayloadCont(lpc._2.payload,
                             addSubBranches(lpc._2.cont, from, label, payload)))
      })
    }
    case c: Choice => { // Note: c might be a branch from a role != @from
      val choices2 = c.choices.map { lpc =>
        (lpc._1, PayloadCont(lpc._2.payload,
                             addSubBranches(lpc._2.cont, from, label, payload)))
      }
      c match {
        // Preserve the choice type (branch/selection)
        case Branch(fr, _) => Branch(fr, choices2)
        case Select(to, _) => Select(to, choices2)
      }
    }
    case Rec(recvar, body) => Rec(recvar,
                                  addSubBranches(body, from, label, payload))
    case rv @ RecVar(_) => rv
  }

  protected[raw]
  def rawToMPST(t: MPST): Option[mpstk.MPST] = t match {
    case End => Some(mpstk.End)
    case c: Choice => {
      val cseq = c.choices.toSeq
      if (cseq.isEmpty) None
      else if (!distinctLabels(cseq)) None
      else {
        val choices2 = for {
          lpc <- c.choices
          pc2 <- rawToPayloadCont(lpc._2)
        } yield (lpc._1, pc2)
        if (choices2.size != cseq.size) {
          // Some choice has an invalid continuation
          None
        }
        Some(c match {
          // Preserve the choice type (branch/selection)
          case Branch(from, _) => mpstk.Branch(from, Map(choices2.toSeq:_*))
          case Select(to, _) => mpstk.Select(to, Map(choices2.toSeq:_*))
        })
      }
    }
    case Rec(recvar, body) => for {
      b2 <- rawToMPST(body)
    } yield mpstk.Rec(mpstk.RecVar(recvar.name), b2)
    case RecVar(name) => Some(mpstk.RecVar(name))
  }

  private def distinctLabels(choices: Seq[(Label, PayloadCont)]): Boolean = {
    val labels = choices.map { _._1 }
    labels.size != labels.distinct.size
  }

  private def rawToPayloadCont(pc: PayloadCont): Option[mpstk.PayloadCont] = {
    for {
      cont2 <- rawToMPST(pc.cont)
    } yield mpstk.PayloadCont(pc.payload, cont2)
  }

  def mpstToRaw(t: mpstk.MPST): MPST = t match {
    case mpstk.End => End
    case c: mpstk.Choice => {
      val choices2 = c.choices.foldLeft(Set[(Label, PayloadCont)]()) {
        (acc, lpc) => acc + ((lpc._1, PayloadCont(lpc._2.payload,
                                                  mpstToRaw(lpc._2.cont))))
      }
      c match {
        // Preserve the choice type (branch/selection)
        case mpstk.Branch(from, _) => Branch(from, choices2)
        case mpstk.Select(to, _) => Select(to, choices2)
      }
    }
    case mpstk.Rec(mpstk.RecVar(name), body) => Rec(RecVar(name),
                                                    mpstToRaw(body))
    case mpstk.RecVar(name) => RecVar(name)
  }
}
