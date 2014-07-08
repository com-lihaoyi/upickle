package upickle
import utest._
import scala.concurrent.duration._
import TestUtil._
import Implicits._
import scala.reflect.ClassTag

object StructTests extends TestSuite{

  val tests = TestSuite{
//    'arrays{
//      'empty-rwk(Array[Int](), "[]")(_.toSeq)
//      'Boolean-rwk(Array(true, false), "[true, false]")(_.toSeq)
//      'Int-rwk(Array(1, 2, 3, 4, 5), "[1, 2, 3, 4, 5]")(_.toSeq)
//      'String-rwk(Array("omg", "i am", "cow"), """["omg", "i am", "cow"]""")(_.toSeq)
//    }
//
//    'tuples{
//      "2" - rw((1, 2, 3.0), "[1, 2, 3.0]", "[1, 2, 3]")
//      "2-1" - rw((false, 1), "[false, 1]")
//      "3" - rw(("omg", 1, "bbq"), """["omg", 1, "bbq"]""")
//    }
//
//    'seqs{
//      'Seq{
//        rw(Seq(true, false), "[true, false]")
//        rw(Seq(): Seq[Int], "[]")
//      }
//      'Vector{
//        rw(Vector(1, 2, 3, 4, 5), "[1, 2, 3, 4, 5]")
//        rw(Vector.empty[Int], "[]")
//      }
//      'List{
//        rw(List("omg", "i am", "cow"), """["omg", "i am", "cow"]""")
//        rw(List(): List[String], "[]")
//        rw(Nil: List[List[Int]], "[]")
//      }
//      'Set-rw(Set("omg", "i am", "cow"), """["omg", "i am", "cow"]""")
//      'SortedSet-rw(collection.SortedSet("omg", "i am", "cow"), """["cow", "i am", "omg"]""")
//      'Map-rw(Map(Nil -> List(1), List(1) -> List(1, 2, 3)), "[[[], [1]], [[1], [1, 2, 3]]]")
//    }
//
//    'option{
//      'Some-rw(Some(123), "[123]")
//      'None-rw(None: Option[String], "[]")
//      'Option{
//        rw(Some(123): Option[Int], "[123]")
//        rw(None: Option[Int], "[]")
//      }
//    }
//
//    'either{
//      'Left-rw(Left(123): Left[Int, Int], "[0, [123]]")
//      'Right-rw(Right(123): Right[Int, Int], "[1, [123]]")
//      'Either{
//        rw(Left(123): Either[Int, Int], "[0, [123]]")
//        rw(Right(123): Either[Int, Int], "[1, [123]]")
//      }
//    }
//
//    'durations{
//      'inf-rw(Duration.Inf, """ "inf" """)
//      "-inf" - rw(Duration.MinusInf, """ "-inf" """)
//      'undef-rw(Duration.Undefined, """ "undef" """)
//      "1-second" - rw(1.second, """1000000000""")
//      "2-hour" - rw(2.hours, """7200000000000""")
//    }
//
//    'combinations{
//      'SeqListMapOptionString-rw[Seq[List[Map[Option[String], String]]]](
//        Seq(Nil, List(Map(Some("omg") -> "omg"), Map(Some("lol") -> "lol", None -> "")), List(Map())),
//        """[[], [[[["omg"], "omg"]], [[["lol"], "lol"], [[], ""]]], [[]]]"""
//      )
//
//      'tuples-rw(
//        (1, (2.0, true), (3.0, 4.0, 5.0)),
//        """[1, [2.0, true], [3.0, 4.0, 5.0]]""",
//        """[1, [2, true], [3, 4, 5]]"""
//      )
//
//      'EitherDurationOptionDuration{
//        rw(Left(10 seconds): Either[Duration, Int], """[0, [10000000000]]""")
//        rw(Right(Some(0.33 millis)): Either[Int, Option[Duration]], """[1, [[330000]]]""")
//        rw(Left(10 seconds): Either[Duration, Option[Duration]], """[0, [10000000000]]""")
//        rw(Right(Some(0.33 millis)): Either[Duration, Option[Duration]], """[1, [[330000]]]""")
//      }
//    }
//
//    'transmutation{
//      'vectorToList{
//        val vectorToList = read[Seq[Double]](write(Vector(1.1, 2.2, 3.3)))
//        assert(
//          vectorToList.isInstanceOf[List[Double]],
//          vectorToList == List(1.1, 2.2, 3.3)
//        )
//
//      }
//      'listToMap{
//        val listToMap = read[Map[Int, String]](write(List((1, "1"), (2, "2"))))
//        assert(
//          listToMap.isInstanceOf[Map[Int, String]],
//          listToMap == Map(1 -> "1", 2 -> "2")
//        )
//      }
//
//    }
//    'caseClasses{
//      case class Box(i: Double)
//      case class Pairing(i: Int, s: String)
//      case class Trilobyte(b: Boolean, a: Float, t: (Int, Int))
//
//      implicit val boxPickler = Case1ReadWriter(Box.apply, Box.unapply)
//      implicit val pairingPickler = Case2ReadWriter(Pairing.apply, Pairing.unapply)
//      implicit val trilobytePickler = Case3ReadWriter(Trilobyte.apply, Trilobyte.unapply)
//
//      rw(Pairing(1, "omg"), "[1, \"omg\"]")
//      rw(Box(1.02), "[1.02]")
//      rw(
//        Trilobyte(true, 3, (5, 6)),
//        "[true, 3.0, [5, 6]]",
//        "[true, 3, [5, 6]]"
//      )
//    }
//
//    'sealedTraits{
//
//      sealed trait T
//      object T{
//        case class A(i: Int) extends T
//        case class B(s: String) extends T
//        case object C extends T
//      }
//
//      val a: T = T.A(1)
//      val b: T = T.B("omg")
//      val c: T = T.C
//
//      val x = knotRW{implicit i: RWKnot[T] => sealedRW(
//        Case1ReadWriter(T.A.apply, T.A.unapply),
//        Case1ReadWriter(T.B.apply, T.B.unapply),
//        Case0ReadWriter(T.C),
//        i
//      )}
//      implicit val (tRW, aRW, bRW, cRW) = x
//
//      rw(a, "[0, [1]]")
//      rw(T.A(2), "[0, [2]]")
//      rw(b, "[1, [\"omg\"]]")
//      rw(T.B("wtf"), "[1, [\"wtf\"]]")
//      rw(c, "[2, []]")
//      rw(T.C, "[2, []]")
//    }
//    'recursiveADT{
//      sealed trait ConsList
//      case class Cons(i: Int, next: ConsList) extends ConsList
//      case object End extends ConsList
//
//      val x = knotRW{implicit i: RWKnot[ConsList] => sealedRW(
//        Case2ReadWriter(Cons.apply, Cons.unapply),
//        Case0ReadWriter(End),
//        i
//      )}
//      implicit val (cLRW, cRW, eRW) = x
//      val c = Cons(5, Cons(6, End))
//      val cl: ConsList = c
//      val serialized = "[0, [5, [0, [6, [1, []]]]]]"
//      assert(
//        write(c) == serialized,
//        c == read[ConsList](serialized),
//        c == read[Cons](serialized),
//        write(cl) == serialized,
//        cl == read[ConsList](serialized),
//        cl == read[Cons](serialized)
//      )
//    }
    'macros{
      case class ADT(i: Int)

      rw(ADT(1), """[1]""")
    }
  }
}
