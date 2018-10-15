// mpstk - the Multiparty Session Types toolKit
// Copyright 2018 Alceste Scalas <alceste.scalas @ imperial.ac.uk>
// Released under the MIT License: https://opensource.org/licenses/MIT
package mpstk

/** Implementation of most operations on session types and global types */
package object ops {
  /** Barendregt convention: return a MPST with unique recursion vars */
  def barendregt(t: MPST): MPST = impl.barendregt(t, Set.empty)._1

  /** Barendregt convention: return a typing context where all types
    * have unique recursion vars */
  def barendregt(ctx: Context): Context = impl.barendregt(ctx)

  /** Set of bound variables */
  def bv(t: Type): Set[RecVar] = impl.bv(t)

  /** Is the type closed? */
  def closed(t: Type): Boolean = fv(t).isEmpty

  /** Set of free variables */
  def fv(t: Type): Set[RecVar] = impl.fv(t)

  /** Is the type guarded? */
  def guarded(t: Type): Boolean = impl.guarded(t, false)
  def guarded(t: GlobalType): Boolean = impl.guarded(t, false)

  /** Set of roles that (might) interact in a global type */
  def roles(t: GlobalType): Set[Role] = impl.roles(t)

  /** Substitute a recursion variable with a replacement MPST */
  def subst(t: MPST, recvar: RecVar, replacement: MPST): MPST = {
    impl.subst(t, recvar, replacement)
  }

  /** Merge (if possible) the two given MPSTs */
  def merge(t1: MPST, t2: MPST): Either[String, MPST] = impl.merge(t1, t2)

  /** Return the potential outputs of the given MPST, towards role @to,
    * as pairs of label and payload.
    */
  def outputs(t: MPST, to: Role): Set[(Label, Type)] = impl.outputs(t, to)

  /** Unfold a recursive session type (or return as-is if non-recursive) */
  def unfold(t: MPST): MPST = impl.unfold(t)

  /** Unfold payload types, ensuring they do not have variables bound by the
    * carrier type
    */
  def unfoldPayloads(t: MPST): MPST = impl.unfoldPayloads(t, Map())

  /** Are the given types in the subtyping relation? */
  def subtypes(t1: Type, t2: Type): Boolean = {
    subtyping.subtypes(t1, t2, Set.empty)
  }

  /** Are the given typing contexts in the subtyping relation? */
  def subtypes(c1: Context, c2: Context): Boolean = {
    (c1.keySet == c2.keySet) && c1.keySet.forall { chan =>
      subtyping.subtypes(c1(chan), c2(chan), Set.empty)
    }
  }

  /** Project a global type onto the given role */
  def projection(g: GlobalType, r: Role): Either[String, MPST] = {
    impl.projection(g, r)
  }
}

package object impl {
  // Alpha-convert bound variables to be unique, and distinct from free
  // variables, using the given set of known (previously-seen) variables.
  // Return both the new MPST, and the set of known variables in it
  protected[mpstk]
  def barendregt(t: MPST, kvars: Set[RecVar]): (MPST, Set[RecVar]) = t match {
    case End => (End, kvars)
    case t: Choice => {
      // This mutable set will be updated while updating the choices
      val kvarsM = scala.collection.mutable.Set[RecVar](kvars.toSeq:_*)
      val choices2 = t.choices.map { lc =>
        val payload2 = lc._2.payload match {
          case p: NonRecursiveType => p
          case p: MPST => {
            val (p2, kvars2) = barendregt(p, Set() ++ kvarsM)
            kvarsM ++= kvars2
            p2
          }
        }
        val (cont2, kvars2) = barendregt(lc._2.cont, Set() ++ kvarsM)
        kvarsM ++= kvars2
        (lc._1, PayloadCont(payload2, cont2))
      }
      // Let's return the same kind of choice (branch or select)
      t match {
        case Branch(from, _) => (Branch(from, choices2), Set() ++ kvarsM)
        case Select(to, _) => (Select(to, choices2), Set() ++ kvarsM)
      }
    }
    case Rec(recvar, body) => {
      if (kvars.contains(recvar)) {
        // We add a "B" suffix to the new recursion var name, and try again.
        // If the newly-generated var is already in kvars, then the recursive
        // call will add another "B" suffix, and so on
        val newvar = RecVar(recvar.name + "B")
        barendregt(Rec(newvar, subst(body, recvar, newvar)), kvars)
      } else {
        val (body2, kvars2) = barendregt(body, kvars + recvar)
        (Rec(recvar, body2), kvars2)
      }
    }
    case r: RecVar => (r, kvars)
  }

  // Barendregt convention: return a typing context where all types
  // have unique recursion vars
  protected[mpstk]
  def barendregt(ctx: Context): Context = {
    Context(ctx.map { ct => (ct._1, ct._2.barendregt) }.toSeq:_* )
  }

  // Compute the set of bound variables in the given type
  protected[mpstk]
  def bv(t: Type): Set[RecVar] = t match {
    case _: NonRecursiveType => Set.empty
    case t: Choice => {
      t.choices.values.foldLeft(Set[RecVar]()) { (acc, c) =>
        acc ++ bv(c.payload) ++ bv(c.cont)
      }
    }
    case Rec(recvar, body) => bv(body) + recvar
    case r: RecVar => Set.empty
  }

  // Compute the set of free variables in the given type
  protected[mpstk]
  def fv(t: Type): Set[RecVar] = t match {
    case _: NonRecursiveType => Set.empty
    case t: Choice => {
      t.choices.values.foldLeft(Set[RecVar]()) { (acc, c) =>
        acc ++ fv(c.payload) ++ fv(c.cont)
      }
    }
    case Rec(recvar, body) => fv(body) - recvar
    case r: RecVar => Set(r)
  }

  // Is the type guarded? @needsGuard tells whether we need to find a
  // branch/selection before a recursion variable
  protected[mpstk]
  def guarded(t: Type, needsGuard: Boolean): Boolean = t match {
    case _: NonRecursiveType => true
    case c: Choice => c.choices.values.forall { pc =>
      guarded(pc.payload, false) && guarded(pc.cont, false)
    }
    case Rec(_, body) => guarded(body, true)
    case RecVar(_) => !needsGuard
  }

  // FIXME: can we refactor to avoid code duplication?
  protected[mpstk]
  def guarded(t: GlobalType, needsGuard: Boolean): Boolean = t match {
    case GlobalType.Comm(_, _, choices) => choices.values.forall { pc =>
      guarded(pc.payload, false) && guarded(pc.cont, false)
    }
    case GlobalType.Rec(_, body) => guarded(body, true)
    case GlobalType.RecVar(_) => !needsGuard
    case GlobalType.End => true
  }

  // Substitute a recursion variable with a replacement MPST
  protected[mpstk]
  def subst(t: MPST, recvar: RecVar, repl: MPST): MPST = t match {
    case End => End
    case Branch(from, choices) => Branch(from, choices.map { lpc =>
      (lpc._1, subst(lpc._2, recvar, repl))
    })
    case Select(to, choices) => Select(to, choices.map { lpc =>
      (lpc._1, subst(lpc._2, recvar, repl))
    })
    case rec @ Rec(rv, body) => {
      if (rv == recvar) rec
      else Rec(rv, subst(body, recvar, repl))
    }
    case rv @ RecVar(_) => {
      if (rv == recvar) repl
      else rv
    }
  }

  // Substitution within payload/continuation pairs
  private
  def subst(pc: PayloadCont, recvar: RecVar, repl: MPST): PayloadCont = {
    val payload2 = pc.payload match {
      case _: NonRecursiveType => pc.payload
      case p: MPST => subst(p, recvar, repl)
    }
    PayloadCont(payload2, subst(pc.cont, recvar, repl))
  }

  // Return the potential outputs towards role @t in the given MPST, as pairs
  // of label and payload.
  protected[mpstk]
  def outputs(t: MPST, to: Role): Set[(Label, Type)] = t match {
    case End => Set.empty
    case Branch(from, choices) => outputs(choices, to)
    case Select(to2, choices) if to == to2 => {
      (choices.toSeq.map { lpc => (lpc._1, lpc._2.payload) }.toSet
       ++ outputs(choices, to))
    }
    case Select(_, choices) => outputs(choices, to) // Select.to != to
    case Rec(_, body) => outputs(body, to)
    case RecVar(_) => Set.empty
  }

  private
  def outputs(choices: Choices[PayloadCont], to: Role): Set[(Label, Type)] = {
    choices.foldLeft(Set[(Label, Type)]()) { (acc, pc) =>
      acc ++ outputs(pc._2.cont, to)
    }
  }

  // Unfold a recursive session type (or return as-is if non-recursive)
  protected[mpstk]
  def unfold(t: MPST): MPST = t match {
    case r @ Rec(recvar, body) => subst(body, recvar, r)
    case other => other
  }

  // Unfold payload types, ensuring they do not have variables bound by the
  // carrier type.  The `env` argument collects bound recursion variables,
  // mapping them to their binding session type: the latter replaces the former
  // in payload types, and the result is a targeted unfolding
  protected[mpstk]
  def unfoldPayloads(t: MPST, env: Map[RecVar, Rec]): MPST = t match {
    case End => End
    case t: Choice => {
      val choices2 = t.choices.map { lpc =>
        val payload2 = lpc._2.payload match {
          case payload: MPST => {
            env.foldLeft(payload) {
              (p, envkv) => envkv match {
                case (recvar, bindt) => subst(p, recvar, bindt)
              }
            }
          }
          case other => other
        }
        (lpc._1, PayloadCont(payload2, unfoldPayloads(lpc._2.cont, env)))
      }
      // Let's return the same kind of choice (branch or select)
      t match {
        case Branch(from, _) => Branch(from, choices2)
        case Select(to, _) => Select(to, choices2)
      }
    }
    case rec @ Rec(recvar, body) => {
      // It's OK if recvar is redefined, and shadows an existing var in env
      Rec(recvar, unfoldPayloads(body, env + (recvar -> rec)))
    }
    case rv @ RecVar(_) => rv
  }

  // Project a global type onto a role.
  protected[mpstk]
  def projection(g: GlobalType, r: Role): Either[String, MPST] = g match {
    case GlobalType.End => Right(End)
    case GlobalType.Comm(from, to, choices) if (from == r) => for {
      choices2 <- projection(choices, r)
    } yield Select(to, choices2)
    case GlobalType.Comm(from, to, choices) if (to == r) => for {
      choices2 <- projection(choices, r)
    } yield Branch(from, choices2)
    case GlobalType.Comm(from, to, choices) => for { // from != r and to != r
      choices2 <- projection(choices, r)
      merged <- merge(choices2.map(_._2.cont).toSeq)
    } yield merged
    case GlobalType.Rec(recvar, body) => for {
      bproj <- projection(body, r)
      projection = bproj match {
        case RecVar(_) => End // Otherwise, we would have a non-guarded type
        case t => Rec(RecVar(recvar.name), t)
      }
    } yield projection
    case GlobalType.RecVar(name) => Right(RecVar(name))
  }

  private def projection(choices: Choices[GlobalType.PayloadCont],
                         r: Role): Either[String, Choices[PayloadCont]] = for {
    lpc2s <- mpstk.util.eitherList(choices.map { lpc =>
      for {
        cont <- projection(lpc._2.cont, r)
      } yield (lpc._1, PayloadCont(lpc._2.payload, cont))
    }.toList)
  } yield Map(lpc2s:_*)

  /** Merge (if possible) the two given MPSTs */
  def merge(t1: MPST, t2: MPST): Either[String, MPST] = t1 match {
    case bra1 @ Branch(from, choices) => t2 match {
      case bra2 @ Branch(from2, choices2) if (from2 == from) => {
        val thisLabels = choices.keySet
        val thatLabels = choices2.keySet

        val myLabels = thisLabels.diff(thatLabels)
        val commonLabels = thisLabels.intersect(thatLabels)
        val othLabels = thatLabels.diff(thisLabels)

        val myChoices = choices.filter { lpc => myLabels.contains(lpc._1) }
        val othChoices = choices2.filter { lpc => othLabels.contains(lpc._1) }
        for {
          commonChoices <- merge(bra1, bra2, commonLabels)
        } yield Branch(from, myChoices ++ commonChoices ++ othChoices)
      }
      case _ => Left(s"Cannot merge: ${t1}, ${t2}")
    }
    case sel1 @ Select(to, choices) => t2 match {
      case sel2 @ Select(to2, choices2) if (to2 == to) => {
        if (choices.keySet != choices2.keySet) None
        for {
          choices2 <- merge(sel1, sel2, choices.keySet)
        } yield Select(to, choices2)
      }
      case _ => Left(s"Cannot merge: ${t1}, ${t2}")
    }
    case rec1 @ Rec(recvar, body) => t2 match {
      case Rec(recvar2, body2) if (recvar2 == recvar) => body.merge(body2)
      case _ => Left(s"Cannot merge: ${t1}, ${t2}")
    }
    case recvar1 @ RecVar(name) => t2 match {
      case RecVar(name2) if (name2 == name) => Right(recvar1)
      case _ => Left(s"Cannot merge: ${t1}, ${t2}")
    }
    case _: End.type => t2 match {
      case _: End.type => Right(End)
      case _ => Left(s"Cannot merge: ${t1}, ${t2}")
    }
  }

  // Utility method: merge the given choices, but using the
  // given set of labels, only, and ensuring their compatibility.
  // NOTE: both @t1 and @t2 must contain all @labels
  private def
  merge(t1: Choice, t2: Choice,
        labels: Set[Label]): Either[String, Choices[PayloadCont]] = for {
    newChoices <- util.eitherList(labels.toList.map { l =>
      val payload1 = t1.choices(l).payload
      val payload2 = t2.choices(l).payload
      if (!(payload1 =:= payload2)) Left(
        s"Unmergeable payload types for message ${l}: ${payload1}, ${payload2}"
      )
      else for {
        cont2 <- merge(t1.choices(l).cont, t2.choices(l).cont)
      } yield (l, PayloadCont(t1.choices(l).payload, cont2))
    }.toList)
  } yield Map(newChoices:_*)

  // Merge (if possible) a sequence of MPSTs
  private def merge(types: Seq[MPST]): Either[String, MPST] = types match {
    case Nil => Left("Cannot merge empty list of types")
    case hd :: tl => tl.foldLeft(Right(hd): Either[String, MPST]) { (acc, t) =>
      for {
        tm <- acc
        merged <- tm.merge(t)
      } yield merged
    }
  }

  // Set of roles that (might) interact in a global type
  protected[mpstk]
  def roles(t: GlobalType): Set[Role] = t match {
    case GlobalType.Comm(from, to, choices) => {
      choices.foldLeft(Set[Role]()) {
        (acc, lpc) => acc ++ roles(lpc._2.cont)
      } ++ Set(from, to)
    }
    case GlobalType.Rec(_, body) => roles(body)
    case GlobalType.RecVar(_) => Set.empty
    case GlobalType.End => Set.empty
  }
}
