package upickle
import utest._
import scala.concurrent.duration._
import TestUtil._
import Implicits._
import scala.reflect.ClassTag

// These guys all have to be out here because uPickle doesn't
// support pickling local classes and objects
object ADTs {
  case class ADTa(i: Int)
  case class ADTb(i: Int, s: String)
  case class ADTc(i: Int, s: String, t: (Double, Double))
  case class ADTd(i: Int, s: String, t: (Double, Double), a: ADTa)
  case class ADTe(i: Int, s: String, t: (Double, Double), a: ADTa, q: Seq[Double])
  case class ADTf(i: Int, s: String, t: (Double, Double), a: ADTa, q: Seq[Double], o: Option[Option[Boolean]])
}
object Hierarchy {
  sealed trait A
  case class B(i: Int) extends A
  case class C(s1: String, s2: String) extends A
}
object Singletons{
  sealed trait A
  case object B extends A
  case object C extends A
}
object Generic{
  case class A[T](t: T)
  case class ADT[A, B, C, D, E, F](a: A, b: B, c: C, d: D, e: E, f: F)
}
object Recursive//{
  sealed trait LL
  case object End  extends LL
  case class Node(c: Int, next: LL) extends LL
//}
object StructTests extends TestSuite{

  val tests = TestSuite{
    'arrays{
      'empty-rwk(Array[Int](), "[]")(_.toSeq)
      'Boolean-rwk(Array(true, false), "[true, false]")(_.toSeq)
      'Int-rwk(Array(1, 2, 3, 4, 5), "[1, 2, 3, 4, 5]")(_.toSeq)
      'String-rwk(Array("omg", "i am", "cow"), """["omg", "i am", "cow"]""")(_.toSeq)
    }

    'tuples{
      "2" - rw((1, 2, 3.0), "[1, 2, 3.0]", "[1, 2, 3]")
      "2-1" - rw((false, 1), "[false, 1]")
      "3" - rw(("omg", 1, "bbq"), """["omg", 1, "bbq"]""")
    }

    'seqs{
      'Seq{
        rw(Seq(true, false), "[true, false]")
        rw(Seq(): Seq[Int], "[]")
      }
      'Vector{
        rw(Vector(1, 2, 3, 4, 5), "[1, 2, 3, 4, 5]")
        rw(Vector.empty[Int], "[]")
      }
      'List{
        rw(List("omg", "i am", "cow"), """["omg", "i am", "cow"]""")
        rw(List(): List[String], "[]")
        rw(Nil: List[List[Int]], "[]")
      }
      'Set-rw(Set("omg", "i am", "cow"), """["omg", "i am", "cow"]""")
      'SortedSet-rw(collection.SortedSet("omg", "i am", "cow"), """["cow", "i am", "omg"]""")
      'Map-rw(Map(Nil -> List(1), List(1) -> List(1, 2, 3)), "[[[], [1]], [[1], [1, 2, 3]]]")
    }

    'option{
      'Some-rw(Some(123), "[123]")
      'None-rw(None: Option[String], "[]")
      'Option{
        rw(Some(123): Option[Int], "[123]")
        rw(None: Option[Int], "[]")
      }
    }

    'either{
      'Left-rw(Left(123): Left[Int, Int], """[0, {"a": 123}]""")
      'Right-rw(Right(123): Right[Int, Int], """[1, {"b": 123}]""")
      'Either{
        rw(Left(123): Either[Int, Int], """[0, {"a": 123}]""")
        rw(Right(123): Either[Int, Int], """[1, {"b": 123}]""")
      }
    }

    'durations{
      'inf-rw(Duration.Inf, """ "inf" """)
      "-inf" - rw(Duration.MinusInf, """ "-inf" """)
      'undef-rw(Duration.Undefined, """ "undef" """)
      "1-second" - rw(1.second, """1000000000""")
      "2-hour" - rw(2.hours, """7200000000000""")
    }

    'combinations{
      'SeqListMapOptionString-rw[Seq[List[Map[Option[String], String]]]](
        Seq(Nil, List(Map(Some("omg") -> "omg"), Map(Some("lol") -> "lol", None -> "")), List(Map())),
        """[[], [[[["omg"], "omg"]], [[["lol"], "lol"], [[], ""]]], [[]]]"""
      )

      'tuples-rw(
        (1, (2.0, true), (3.0, 4.0, 5.0)),
        """[1, [2.0, true], [3.0, 4.0, 5.0]]""",
        """[1, [2, true], [3, 4, 5]]"""
      )

      'EitherDurationOptionDuration{
        rw(Left(10 seconds): Either[Duration, Int], """[0, {"a": 10000000000}]""")
        rw(Right(Some(0.33 millis)): Either[Int, Option[Duration]], """[1, {"b": [330000]}]""")
        rw(Left(10 seconds): Either[Duration, Option[Duration]], """[0, {"a": 10000000000}]""")
        rw(Right(Some(0.33 millis)): Either[Duration, Option[Duration]], """[1, {"b": [330000]}]""")
      }
    }

    'transmutation{
      'vectorToList{
        val vectorToList = read[Seq[Double]](write(Vector(1.1, 2.2, 3.3)))
        assert(
          vectorToList.isInstanceOf[List[Double]],
          vectorToList == List(1.1, 2.2, 3.3)
        )

      }
      'listToMap{
        val listToMap = read[Map[Int, String]](write(List((1, "1"), (2, "2"))))
        assert(
          listToMap.isInstanceOf[Map[Int, String]],
          listToMap == Map(1 -> "1", 2 -> "2")
        )
      }

    }
    'caseClasses{
      case class Box(i: Double)
      case class Pairing(i: Int, s: String)
      case class Trilobyte(b: Boolean, a: Float, t: (Int, Int))

      implicit val boxPickler = Case1ReadWriter(Box.apply, Box.unapply, Seq("i"))
      implicit val pairingPickler = Case2ReadWriter(Pairing.apply, Pairing.unapply, Seq("i", "s"))
      implicit val trilobytePickler = Case3ReadWriter(Trilobyte.apply, Trilobyte.unapply, Seq("b", "a", "t"))

      rw(Box(1.02), """{"i": 1.02}""")
      rw(Pairing(1, "omg"), """{"i": 1, "s": "omg"}""")
      rw(
        Trilobyte(true, 3, (5, 6)),
        """{"b": true, "a": 3.0, "t": [5, 6]}""",
        """{"b": true, "a": 3, "t": [5, 6]}"""
      )
    }

    'sealedTraits{

      sealed trait T
      object T{
        case class A(i: Int) extends T
        case class B(s: String) extends T
        case object C extends T
      }

      val a: T = T.A(1)
      val b: T = T.B("omg")
      val c: T = T.C

      val x = knotRW{implicit i: RWKnot[T] => sealedRW(
        Case1ReadWriter(T.A.apply, T.A.unapply, Seq("i")),
        Case1ReadWriter(T.B.apply, T.B.unapply, Seq("s")),
        Case0ReadWriter(T.C),
        i
      )}
      implicit val (tRW, aRW, bRW, cRW) = x

      rw(a, """[0, {"i": 1}]""")
      rw(T.A(2), """[0, {"i": 2}]""")
      rw(b, """[1, {"s": "omg"}]""")
      rw(T.B("wtf"), """[1, {"s": "wtf"}]""")
      rw(c, "[2, []]")
      rw(T.C, "[2, []]")
    }
    'recursiveADT{
      sealed trait ConsList
      case class Cons(i: Int, next: ConsList) extends ConsList
      case object End extends ConsList

      val x = knotRW{implicit i: RWKnot[ConsList] => sealedRW(
        Case2ReadWriter(Cons.apply, Cons.unapply, Seq("i", "next")),
        Case0ReadWriter(End),
        i
      )}
      implicit val (cLRW, cRW, eRW) = x
      val c = Cons(5, Cons(6, End))
      val cl: ConsList = c
      val serialized = """[0, {"i": 5, "next": [0, {"i": 6, "next": [1, []]}]}]"""

      assert(
        write(c) == serialized,
        c == read[ConsList](serialized),
        c == read[Cons](serialized),
        write(cl) == serialized,
        cl == read[ConsList](serialized),
        cl == read[Cons](serialized)
      )
    }

    'macros{
      'simpleAdt {
        import ADTs._

        rw(ADTa(1), """{"i": 1}""")(macroRW, macroRW)
        rw(ADTb(1, "lol"), """{"i": 1, "s": "lol"}""")
        rw(ADTc(1, "lol", (1.1, 1.2)), """{"i": 1, "s": "lol", "t": [1.1, 1.2]}""")
        rw(
          ADTd(1, "lol", (1.1, 1.2), ADTa(1)),
          """{"i": 1, "s": "lol", "t": [1.1, 1.2], "a": {"i": 1}}"""
        )
        rw(
          ADTe(1, "lol", (1.1, 1.2), ADTa(1), List(1.0, 2.0, 3.14)),
          """{"i": 1, "s": "lol", "t": [1.1, 1.2], "a": {"i": 1}, "q": [1.0, 2.0, 3.14]}"""
        )
        rw(
          ADTf(1, "lol", (1.1, 1.2), ADTa(1), List(1.0, 2.0, 3.14), Some(None)),
          """{"i": 1, "s": "lol", "t": [1.1, 1.2], "a": {"i": 1}, "q": [1.0, 2.0, 3.14], "o": [[]]}"""
        )
      }
      'adtTree{
        import Hierarchy._

        rw(B(1), """[0, {"i": 1}]""")
        rw(C("a", "b"), """[1, {"s1": "a", "s2": "b"}]""")
//
        rw(B(1): A, """[0, {"i": 1}]""")(macroRW, macroRW)
        rw(C("a", "b"): A, """[1, {"s1": "a", "s2": "b"}]""")
      }
      'singleton{
        import Singletons._
        
        rw(Singletons.B, """[0, []]""")(macroRW, macroRW)
        rw(Singletons.C, """[1, []]""")
        rw(Singletons.B: Singletons.A, """[0, []]""")
        rw(Singletons.C: Singletons.A, """[1, []]""")
      }
      'generic{
        'simple {
          import Generic.A
          rw(A(1), """{"t": 1}""")(macroRW, macroRW)
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
//      'recursive{
//        import Recursive._
//        rw(End: LL, """[0, []]""")(macroRW, macroRW)
//        rw(Node(3, End): LL, """[1, {"c": 3, "next":[0, []]}]""")(macroRW, macroRW)
//        rw(Node(6, Node(3, End)), """[1, {"c": 6, "next": [1, {"c": 3, "next": [0, []]}]}]""")
//      }
    }
  }
}
