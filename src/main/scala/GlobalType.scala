package mpstk

/** Global type */
sealed abstract class GlobalType {
  /** Get a global typing context from the global type, if projectable.
    *
    * @param s the session of each entry
    */
  def context(s: Session): Either[String, Context] = {
    if (!projectable) {
      val lefts = projections.filter { rp => rp._2.isLeft }
      val errors = lefts.map { rl =>
        s"${rl._1}: ${rl._2.left.get}"
      }.mkString("\n")
      Left(s"The following projections are undefined:\n${errors}")
    } else Right(Context(
      // NOTE: since we are projectable, .right.get below won't fail
      projections.toSeq.map(rt => (Channel(s, rt._1), rt._2.right.get)):_*
    ))
  }

  /** Does the global type have guarded recursion variables, only? */
  lazy val guarded: Boolean = ops.guarded(this)

  /** Set of roles that (might) interact in the global type */
  lazy val roles: Set[Role] = ops.roles(this)

  /** Projection onto a given role */
  def projection(r: Role): Either[String, MPST] = ops.projection(this, r)

  /** Projections on all roles */
  lazy val projections: Map[Role, Either[String, MPST]] = {
    Map(roles.map(r => (r, projection(r))).toSeq:_*)
  }

  /** Is this type projectable on all roles? */
  lazy val projectable: Boolean = roles.forall { r =>
    projections(r).isRight
  }
}

object GlobalType {
  /** Terminated global type */
  case object End extends GlobalType {
    override def toString = "end"
  }

  case class PayloadCont(payload: Type,
                         cont: GlobalType) extends BasePayloadCont[GlobalType](payload, cont)
  /** Communication between two roles */
  case class Comm(from: Role, to: Role,
                  choices: Choices[PayloadCont]) extends GlobalType {
    override def toString = {
      val cs = choices.map { lpc => s"${lpc._1}${lpc._2}" }
      f"""${from}→${to}{${cs.mkString(", ")}}"""
    }
  }

  // FIXME: we can refactor the two classes below, to avoid duplication

  case class Rec(recvar: RecVar, body: GlobalType) extends GlobalType {
    override def toString = s"rec(${recvar})${body}"
  }

  case class RecVar(name: String) extends GlobalType {
    override def toString = s"${name}"
  }
}


