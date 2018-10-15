package mpstk.mcrl2

import mpstk.util.resource

import scala.collection.immutable.SortedSet

/** A property, i.e., an mCRL2 formula to verify [[mpstk.mcrl2.Spec]]s. */
protected[mpstk]
abstract class Property(val description: String, val filename: String,
                        val shortName: String) {
  private lazy val formula = resource("mcrl2/" ++ filename ++ ".mcf")

  /** Return the mCRL2 formula as a string */
  def show: String = formula
}

/** A container for the properties available on mpstk. */
object Properties {
  /** The set of all known properties */
  val all: Set[Property] = SortedSet[Property](
    Safety, DeadlockFreedom, Termination, NeverTermination,
    Liveness, LivenessPlus, LivenessPlusPlus
  )(Ordering.by(_.description))
}

/** Safety property. */
case object Safety extends Property("Safety", "safety", "safe")

/** Deadlock freedom property. */
case object DeadlockFreedom extends Property("Deadlock freedom",
                                             "deadlock-freedom", "df")

/** Termination property. */
case object Termination extends Property("Termination", "termination", "term")

/** Never-termination property. */
case object NeverTermination extends Property("Never-termination",
                                              "never-termination", "nterm")

/** Liveness property. */
case object Liveness extends Property("Liveness", "liveness", "live")

/** Liveness+ property. */
case object LivenessPlus extends Property("Liveness+", "liveness+", "live+")

/** Liveness+ property. */
case object LivenessPlusPlus extends Property("Liveness++",
                                              "liveness++", "live++")
