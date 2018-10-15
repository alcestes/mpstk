// mpstk - the Multiparty Session Types toolKit
// Copyright 2018 Alceste Scalas <alceste.scalas @ imperial.ac.uk>
// Released under the MIT License: https://opensource.org/licenses/MIT
package mpstk

/** Role. */
case class Role(name: String) {
  override def toString = name
}

/** Message label. */
case class Label(name: String) {
  override def toString = name
}

/** Session */
case class Session(name: String) {
  override def toString = s"${name}"
}

/** Channel (with role): pairs a session with the role being played within. */
case class Channel(session: Session, role: Role) {
  override def toString = s"${session}[${role}]"
}

/** Generic type (session type, or ground type). */
sealed abstract class Type {
  def :<(that: Type) = ops.subtypes(this, that)
  def :>(that: Type) = ops.subtypes(that, this)
  def =:=(that: Type) = (this :< that) && (this :> that)
}

/** Marker trait for types that cannot contain recursion variables. */
sealed trait NonRecursiveType extends Type

sealed abstract class GroundType extends NonRecursiveType

/** Namespace for ground types. */
object GroundType {
  case object Bool extends GroundType {
    override def toString = "bool"
  }
  case object Int extends GroundType {
    override def toString = "int"
  }
  case object String extends GroundType {
    override def toString = "string"
  }
  case object Unit extends GroundType {
    override def toString = "unit"
  }
}


/** Multiparty session type. */
sealed abstract class MPST extends Type {
  /** Return a version of this type respecting the Barendregt convention */
  def barendregt: MPST = ops.barendregt(this)

  /** Is the type closed? */
  def closed: Boolean = ops.closed(this)

  /** Free variables. */
  def fv: Set[RecVar] = ops.fv(this)

  /** Does the type have guarded recursion variables, only? */
  def guarded: Boolean = ops.guarded(this)

  /** Merge (if possible) with the given type. */
  def merge(that: MPST): Either[String, MPST] = ops.merge(this, that)

  /** Return the potential outputs towards role @to, as pairs of label
    * and payload.
    */
  def outputs(to: Role): Set[(Label, Type)] = ops.outputs(this, to)

  /** Unfold payload types, ensuring they do not have variables bound by the
    * carrier type.
    */
  def unfoldPayloads: MPST = ops.unfoldPayloads(this)

  /** Convert to raw MPST. */
  protected[mpstk] def toRaw: raw.MPST = raw.ops.mpstToRaw(this)
}

/** Terminated session type */
case object End extends MPST with NonRecursiveType {
  override def toString = "end"
}

/** Session type payload and continuation */
case class PayloadCont(payload: Type,
                       cont: MPST) extends BasePayloadCont[MPST](payload, cont)

/** A generic choice, abstracting branching and selection types */
sealed abstract class Choice(val choices: Choices[PayloadCont]) extends MPST {
  override def toString = {
    val cs = choices.map { lpc => s"${lpc._1}${lpc._2}" }
    cs.mkString(", ")
  }
}

/** Branching type. */
case class Branch(from: Role,
                  override val choices: Choices[PayloadCont]) extends Choice(choices) {
  override def toString = s"${from}&{${super.toString}}"
}

/** Selection type. */
case class Select(to: Role,
                  override val choices: Choices[PayloadCont]) extends Choice(choices) {
  override def toString = s"${to}⊕{${super.toString}}"
}

/** Recursive type. */
case class Rec(recvar: RecVar, body: MPST) extends MPST {
  override def toString = s"μ(${recvar})${body}"
}

/** Recursion variable. */
case class RecVar(name: String) extends MPST {
  override def toString = s"${name}"
}
