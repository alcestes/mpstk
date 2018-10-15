package mpstk.test

import org.scalatest._
import mpstk._

class BVSpec extends FlatSpec with Matchers {
  "bv()" should "return the bound variables of a type" in {
    val t1 = RecVar("x")
    ops.bv(t1) should be (Set())

    val t2 = Rec(RecVar("x"), RecVar("x")) // Non-guarded type
    ops.bv(t2) should be (Set(RecVar("x")))

    val t3 = Rec(RecVar("x"), RecVar("y")) // Non-guarded type
    ops.bv(t3) should be (Set(RecVar("x")))
  }
}

class FVSpec extends FlatSpec with Matchers {
  "fv()" should "return the free variables of a type" in {
    val t1 = RecVar("x")
    ops.fv(t1) should be (Set(RecVar("x")))

    val t2 = Rec(RecVar("x"), RecVar("x")) // Non-guarded type
    ops.fv(t2) should be (Set())

    val t3 = Rec(RecVar("x"), RecVar("y")) // Non-guarded type
    ops.fv(t3) should be (Set(RecVar("y")))
  }
}

class BarendregtSpec extends FlatSpec with Matchers {
  "barendregt()" should "rename bound variables, making them unique" in {
    val t1 = RecVar("x")
    ops.barendregt(t1) should be (t1)

    val t2 = Rec(RecVar("x"), RecVar("x")) // Non-guarded type
    ops.barendregt(t2) should be (t2)

    val t3 = Rec(RecVar("x"), RecVar("y")) // Non-guarded type
    ops.barendregt(t3) should be (t3)

    val t4 = Rec(RecVar("x"), Rec(RecVar("x"), RecVar("x"))) // Non-guarded type
    ops.barendregt(t4) should be (Rec(RecVar("x"), Rec(RecVar("xB"), RecVar("xB"))))
  }
}

class SubtypingSpec extends FlatSpec with Matchers {
  "subtyping" should "be the identity relation for ground types" in {
    val types = List(GroundType.Bool, GroundType.Int, GroundType.String)
    for (ta <- types; tb <- types) {
      ta :< tb should be (ta == tb)
    }
  }

  val t1 = Rec(RecVar("x"),
               Branch(Role("r"),
                      Map(Label("l1") -> PayloadCont(GroundType.Int, RecVar("x")))))

  val t2 = Rec(RecVar("x"),
               Branch(Role("r"),
                      Map(Label("l1") -> PayloadCont(GroundType.Int, RecVar("x")),
                          Label("l2") -> PayloadCont(GroundType.String, RecVar("x")))))

  val t3 = Rec(RecVar("x"),
               Select(Role("r"),
                      Map(Label("l1") -> PayloadCont(GroundType.Int, RecVar("x")))))

  val t4 = Rec(RecVar("x"),
               Select(Role("r"),
                      Map(Label("l1") -> PayloadCont(GroundType.Int, RecVar("x")),
                          Label("l2") -> PayloadCont(GroundType.String, RecVar("x")))))
    
  it should "be contravariant wrt. choices in branching types" in {
    t1 :< t2 should be (true)
    t2 :< t1 should be (false)
  }

  it should "be covariant wrt. choices in selection types" in {
    t3 :< t4 should be (false)
    t4 :< t3 should be (true)
  }

  it should "hold (or not) up-to unfolding" in {
    val types = List(t1, t2, t3, t4)
    for (ta <- types; tb <- types) {
      val reference: Boolean = ta :< tb
      ops.unfold(ta) :< tb should be (reference)
      ta :< ops.unfold(tb) should be (reference)
      ops.unfold(ta) :< ops.unfold(tb) should be (reference)
    }
  }
}

class UnfoldingSpec extends FlatSpec with Matchers {
  val tp = Rec(RecVar("y"),
               Branch(Role("r2"),
                      Map(Label("l2") -> PayloadCont(GroundType.Int,
                                                     RecVar("y")))))
  def recbody(p: MPST) = Select(Role("r"),
                                Map(Label("l") -> PayloadCont(p, RecVar("x"))))
  def rec(p: MPST) = Rec(RecVar("x"), recbody(p))
  
  "unfoldPayloads" should "not change closed payloads" in {
    ops.unfoldPayloads(tp) should be (tp)
    ops.unfoldPayloads(rec(tp)) should be (rec(tp))
  }

  it should "expand non-closed payloads" in {
    ops.unfoldPayloads(rec(RecVar("x"))) should be (rec(rec(RecVar("x"))))
  }

  it should "preserve sub/super-typing" in {
    rec(RecVar("x")) =:= ops.unfoldPayloads(rec(RecVar("x"))) should be (true)
    
  }
}