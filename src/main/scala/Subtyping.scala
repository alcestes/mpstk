package mpstk

protected[mpstk] object subtyping {
  // Check subtyping under a given set of assumptions (pairs of related types)
  def subtypes(t1: Type, t2: Type, asmp: Set[(Type, Type)]): Boolean = t1 match {
    case t1m: MPST => t2 match {
      case t2m: MPST => subtypes(t1m, t2m, asmp)
      case _ => false
    }
    case t1g: GroundType => t2 match {
      case t2g: GroundType => t1g == t2g
      case _ => false
    }
  }

  private def subtypes(t1: MPST, t2: MPST, asmp: Set[(Type, Type)]): Boolean = (t1, t2) match {
    case p if asmp.contains(p) => true
    case p @ (Rec(_,_), _) => subtypes(ops.unfold(p._1), p._2, asmp + p)
    case p @ (_, Rec(_,_)) => subtypes(p._1, ops.unfold(p._2), asmp + p)
    case (End, End) => true
    case b@(Branch(from1, c1), Branch(from2, c2)) => {
      ((from1 == from2) && (c1.keySet subsetOf c2.keySet) &&
       c1.keys.forall { label =>
         val pc1 = c1(label)
         val pc2 = c2(label)
         (subtypes(pc1.payload, pc2.payload, asmp) &&
          subtypes(pc1.cont, pc2.cont, asmp))
       })
    }
    case (Select(to1, c1), Select(to2, c2)) => {
      ((to1 == to2) && (c2.keySet subsetOf c1.keySet) &&
       c2.keys.forall { label =>
         val pc1 = c1(label)
         val pc2 = c2(label)
         (subtypes(pc2.payload, pc1.payload, asmp) &&
          subtypes(pc1.cont, pc2.cont, asmp))
       })
    }
    case _ => false
  }
}