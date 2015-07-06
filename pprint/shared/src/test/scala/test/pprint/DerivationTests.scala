package test.pprint


import utest._
import pprint.Config.Defaults._
import scala.collection.{immutable => imm, mutable}

object DerivationTests extends TestSuite{

  def check[T: pprint.PPrint](t: T, expected: String) = {
    val pprinted = pprint.PPrint(t).mkString
    assert(pprinted == expected.trim)
  }




  val tests = TestSuite{
    'singletons {
      import derive.Singletons._
      check(Standalone, "Standalone")
      check(BB, "BB")
      check(CC, "CC")
      check(CC: AA, "CC")
    }
    'adts {
      import derive.ADTs._
      check(
        ADTb(123, "hello world"),
        """ADTb(123, "hello world")"""
      )

      check(
        Seq(ADTb(123, "hello world"), ADTb(-999, "i am cow")),
        """List(ADTb(123, "hello world"), ADTb(-999, "i am cow"))"""
      )

      check(ADT0(), "ADT0()")
    }
    'sealedHierarchies {
      import derive.DeepHierarchy._
      check(
        AnQ(1),
        "AnQ(1)"
      )
      check(
        AnQ(1): Q,
        "AnQ(1)"
      )
      check(
        E(false),
        "E(false)"
      )
      check(
        F(AnQ(1)): A,
        "F(AnQ(1))"
      )
    }
    'varargs {
      import derive.Varargs._
      check(Sentence("omg", "2", "3"), """Sentence("omg", Array("2", "3"))""")
    }
    'genericADTs {
      import derive.GenericADTs._
      check(DeltaHardcoded.Remove("omg"), """Remove("omg")""")
      check(
        Delta.Insert(List("omg", "wtf"), (1, 0.2)),
        """Insert(List("omg", "wtf"), (1, 0.2))"""
      )
      check(
        DeltaInvariant.Clear[Int, String](),
        """Clear()"""
      )
      check(
        DeltaInvariant.Clear(),
        """Clear()"""
      )

      check(
        DeltaHardcoded.Remove(List(1, 2, 3)): DeltaHardcoded[Seq[Int], String],
        """Remove(List(1, 2, 3))"""
      )
      check(
        Delta.Insert(List("omg", "wtf"), (1, 0.2)): Delta[List[String], (Int, Double)],
        """Insert(List("omg", "wtf"), (1, 0.2))"""
      )
      check(
        DeltaInvariant.Clear(): DeltaInvariant[Int, String],
        """Clear()"""
      )
              check(
                DeltaInvariant.Clear(): DeltaInvariant[Nothing, Nothing],
                """Clear()"""
              )
    }
    'recursive {
      import derive.Recursive._
      check(
        IntTree(1, List(IntTree(2, List()), IntTree(4, List(IntTree(3, List()))))),
        "IntTree(1, List(IntTree(2, List()), IntTree(4, List(IntTree(3, List())))))"
      )
    }
    'exponential{
      import derive.Exponential._

      check(
        A1(A2(A3(null, null), null), A2(null, A3(A4(null, null), null))),
        "A1(A2(A3(null, null), null), A2(null, A3(A4(null, null), null)))"
      )
    }
    'fallback{
      // make sure we can pprint stuff that looks nothing like a case class
      // by falling back to good old toString
      import derive.Amorphous._
      val a =  new A()
      check(a, a.toString)
      check(a: Any, a.toString)
      check(Seq("lol", 1, 'c'), "List(lol, 1, c)")
      check(("lol", 1, 'c'): AnyRef, "(lol,1,c)")
      // Make sure when dealing with composite data structures, we continue
      // to use the static versions as deep as we can go before falling back
      // to toString
      check(
        derive.Generic.ADT('a', 'a':Any, "lol", "lol": Any, (1.5, 2.5), (1.5, 2.5): AnyRef),
        """ADT('a', a, "lol", lol, (1.5, 2.5), (1.5,2.5))"""
      )
    }
  }
}
