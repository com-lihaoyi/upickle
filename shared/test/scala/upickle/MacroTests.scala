package upickle

import utest._
import upickle.TestUtil._
import scala.Some
// These guys all have to be out here because uPickle doesn't
// support pickling local classes and objects
object ADTs {
  case class ADTa(i: Int)
  case class ADTb(i: Int, s: String)
  case class ADTc(i: Int, s: String, t: (Double, Double))
  case class ADTd(i: Int, s: String, t: (Double, Double), a: ADTa)
  case class ADTe(i: Int, s: String, t: (Double, Double), a: ADTa, q: Seq[Double])
  case class ADTf(i: Int, s: String, t: (Double, Double), a: ADTa, q: Seq[Double], o: Option[Option[Boolean]])
  case class ADTz(t1: Int,
                  t2: String,
                  t3: Int,
                  t4: String,
                  t5: Int,
                  t6: String,
                  t7: Int,
                  t8: String,
                  t9: Int,
                  t10: String,
                  t11: Int,
                  t12: String,
                  t13: Int,
                  t14: String,
                  t15: Int,
                  t16: String,
                  t17: Int,
                  t18: String
                   )
}
object Hierarchy {
  sealed trait A
  case class B(i: Int) extends A
  case class C(s1: String, s2: String) extends A
}
object Singletons{
  sealed trait AA
  case object BB extends AA
  case object CC extends AA
}
object Generic{
  case class A[T](t: T)
  case class ADT[A, B, C, D, E, F](a: A, b: B, c: C, d: D, e: E, f: F)
}
object Recursive{
  sealed trait LL
  case object End  extends LL
  case class Node(c: Int, next: LL) extends LL
}
object Annotated {
  sealed trait A
  @key("0") case class B(@key("omg") i: Int) extends A
  @key("1") case class C(@key("lol") s1: String, @key("wtf") s2: String) extends A
}
object MacroTests extends TestSuite{
  val tests = TestSuite{
    'simpleAdt {
      import ADTs._

      rw(ADTs.ADTa(1), """{"i": 1}""")(Reader.macroR, Writer.macroW)
      rw(ADTs.ADTb(1, "lol"), """{"i": 1, "s": "lol"}""")
      rw(ADTc(1, "lol", (1.1, 1.2)), """{"i": 1, "s": "lol", "t": [1.1, 1.2]}""")
      rw(
        ADTd(1, "lol", (1.1, 1.2), ADTa(1)),
        """{"i": 1, "s": "lol", "t": [1.1, 1.2], "a": {"i": 1}}"""
      )

      rw(
        ADTe(1, "lol", (1.1, 1.2), ADTa(1), List(1.2, 2.1, 3.14)),
        """{"i": 1, "s": "lol", "t": [1.1, 1.2], "a": {"i": 1}, "q": [1.2, 2.1, 3.14]}"""
      )
      rw(
        ADTf(1, "lol", (1.1, 1.2), ADTa(1), List(1.2, 2.1, 3.14), Some(None)),
        """{"i": 1, "s": "lol", "t": [1.1, 1.2], "a": {"i": 1}, "q": [1.2, 2.1, 3.14], "o": [[]]}"""
      )
      val chunks = for(i <- 1 to 18) yield{
        val rhs = if (i % 2 == 1) "1" else "\"1\""
        val lhs = '"' + s"t$i" + '"'
        s"$lhs: $rhs"
      }

      val expected = s"""{${chunks.mkString(", ")}}"""

      rw(
        ADTz(1, "1", 1, "1", 1, "1", 1, "1", 1, "1", 1, "1", 1, "1", 1, "1", 1, "1"),
        expected
      )
    }
    'adtTree{
      import Hierarchy._

      rw(B(1), """["upickle.Hierarchy.B", {"i": 1}]""")
      rw(C("a", "b"), """["upickle.Hierarchy.C", {"s1": "a", "s2": "b"}]""")

      rw(Hierarchy.B(1): Hierarchy.A, """["upickle.Hierarchy.B", {"i": 1}]""")
      rw(C("a", "b"): A, """["upickle.Hierarchy.C", {"s1": "a", "s2": "b"}]""")
    }
    'adtTree{
      import Annotated._

      rw(B(1), """["0", {"omg": 1}]""")
      rw(C("a", "b"), """["1", {"lol": "a", "wtf": "b"}]""")

      rw(B(1): A, """["0", {"omg": 1}]""")
      rw(C("a", "b"): A, """["1", {"lol": "a", "wtf": "b"}]""")
    }
    'singleton{
      import Singletons._

      //        rw(BB, """[0, []]""")
      //        rw(BC, """[1, []]""")
      rw(BB: AA, """["upickle.Singletons.BB", []]""")
      rw(CC: AA, """["upickle.Singletons.CC", []]""")
    }
    'generic{
      'simple {
        import Generic.A
        rw(A(1), """{"t": 1}""")
        rw(A("1"), """{"t": "1"}""")
        rw(A(Seq("1", "2", "3")), """{"t": ["1", "2", "3"]}""")
        rw(A(A(A(A(A(A(A(1))))))), """{"t": {"t": {"t": {"t": {"t": {"t": {"t": 1}}}}}}}""")
      }
      'large{
        import Generic.ADT
        rw(ADT(1, 2, 3, 4, 5, 6), """{"a": 1, "b": 2, "c": 3, "d": 4, "e": 5, "f": 6}""")
        rw(
          ADT(
            ADT(1, 2, 3, 4, 5, 6),
            ADT(1, 2, 3, 4, 5, 6),
            ADT(1, 2, 3, 4, 5, 6),
            ADT(1, 2, 3, 4, 5, 6),
            ADT(1, 2, 3, 4, 5, 6),
            ADT(1, 2, 3, 4, 5, 6)
          ),
          """{"a": {"a": 1, "b": 2, "c": 3, "d": 4, "e": 5, "f": 6}, "b": {"a": 1, "b": 2, "c": 3, "d": 4, "e": 5, "f": 6}, "c": {"a": 1, "b": 2, "c": 3, "d": 4, "e": 5, "f": 6}, "d": {"a": 1, "b": 2, "c": 3, "d": 4, "e": 5, "f": 6}, "e": {"a": 1, "b": 2, "c": 3, "d": 4, "e": 5, "f": 6}, "f": {"a": 1, "b": 2, "c": 3, "d": 4, "e": 5, "f": 6}}"""
        )

      }
    }

    'recursive{
      import Recursive._

      rw(End: LL, """["upickle.Recursive.End", []]""")
      rw(Node(3, End): LL, """["upickle.Recursive.Node", {"c": 3, "next": ["upickle.Recursive.End", []]}]""")
      rw(Node(6, Node(3, End)), """["upickle.Recursive.Node", {"c": 6, "next": ["upickle.Recursive.Node", {"c": 3, "next": ["upickle.Recursive.End", []]}]}]""")
    }
  }
}
