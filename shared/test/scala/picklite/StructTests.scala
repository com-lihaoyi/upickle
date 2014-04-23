package picklite
import utest._
import scala.concurrent.duration._
import TestUtil._
object StructTests extends TestSuite{
  val tests = TestSuite{
    "arrays" - {
      "empty" - rwk(Array[Int](), "[]")(_.toSeq)
      "Boolean" - rwk(Array(true, false), "[true, false]")(_.toSeq)
      "Int" - rwk(Array(1, 2, 3, 4, 5), "[1, 2, 3, 4, 5]")(_.toSeq)
      "String" - rwk(Array("omg", "i am", "cow"), """["omg", "i am", "cow"]""")(_.toSeq)
    }

    "tuples" - {
      "2" - rw((1, 2, 3.0), "[1, 2, \"3.0\"]")
      "2-1" - rw((false, 1), "[false, 1]")
      "3" - rw(("omg", 1, "bbq"), """["omg", 1, "bbq"]""")
    }

    "seqs" - {
      "Seq" - {
        rw(Seq(true, false), "[true, false]")
        //        rw(Seq(), "[]")
      }
      "Vector" - {
        rw(Vector(1, 2, 3, 4, 5), "[1, 2, 3, 4, 5]")
        rw(Vector.empty[Int], "[]")
      }
      "List" - {
        rw(List("omg", "i am", "cow"), """["omg", "i am", "cow"]""")
        //        rw(List(), "[]")
        //        rw(Nil, "[]")
      }
      "Set" - rw(Set("omg", "i am", "cow"), """["omg", "i am", "cow"]""")
      "SortedSet" - rw(collection.SortedSet("omg", "i am", "cow"), """["cow", "i am", "omg"]""")
      "Map" - rw(Map(Nil -> List(1), List(1) -> List(1, 2, 3)), "[[[], [1]], [[1], [1, 2, 3]]]")
    }

    "option" - {
      "Some" - rw(Some(123), "[123]")
      "None" - rw(None, "[]")
      "Option" - {
        rw(Some(123): Option[Int], "[123]")
        rw(None: Option[Int], "[]")
      }
    }

    "either" - {
      "Left" - rw(Left(123), "[0, 123]")
      "Right" - rw(Right(123), "[1, 123]")
      "Either" - {
        rw(Left(123): Either[Int, Int], "[0, 123]")
        rw(Right(123): Either[Int, Int], "[1, 123]")
      }
    }

    "durations" - {
      "inf" - rw(Duration.Inf, """ "inf" """)
      "-inf" - rw(Duration.MinusInf, """ "-inf" """)
      "undef" - rw(Duration.Undefined, """ "undef" """)
      "1-second" - rw(1.second, """ "1000000000" """)
      "2-hour" - rw(2.hours, """ "7200000000000" """)
    }

    "combinations" - {
      "SeqListMapOptionString" - rw[Seq[List[Map[Option[String], String]]]](
        Seq(Nil, List(Map(Some("omg") -> "omg"), Map(Some("lol") -> "lol", None -> "")), List(Map())),
        """[[], [[[["omg"], "omg"]], [[["lol"], "lol"], [[], ""]]], [[]]]"""
      )

      "tuples" - rw((1, (2.0, true), (3.0, 4.0, 5.0)), """[1, ["2.0", true], ["3.0", "4.0", "5.0"]]""")

      "EitherDurationOptionDuration" - {
        rw(Left(10 seconds), """[0, "10000000000"]""")
        rw(Right(Some(0.33 millis)), """[1, ["330000"]]""")
        rw(Left(10 seconds): Either[Duration, Option[Duration]], """[0, "10000000000"]""")
        rw(Right(Some(0.33 millis)): Either[Duration, Option[Duration]], """[1, ["330000"]]""")
      }
    }

    "transmutation" - {
      "vectorToList" - {
        val vectorToList = read[Seq[Double]](write(Vector(1.1, 2.2, 3.3)))
        assert(
          vectorToList.isInstanceOf[List[Double]],
          vectorToList == List(1.1, 2.2, 3.3)
        )

      }
      "listToMap" - {
        val listToMap = read[Map[Int, String]](write(List((1, "1"), (2, "2"))))
        assert(
          listToMap.isInstanceOf[Map[Int, String]],
          listToMap == Map(1 -> "1", 2 -> "2")
        )
      }

    }
    "caseClasses" - {
      case class Box(i: Double)
      case class Pairing(i: Int, s: String)
      case class Trilobyte(b: Boolean, a: Float, t: (Int, Int))

      implicit val boxPickler = Case1ReadWriter(Box.apply, Box.unapply)
      implicit val pairingPickler = Case2ReadWriter(Pairing.apply, Pairing.unapply)
      implicit val trilobytePickler = Case3ReadWriter(Trilobyte.apply, Trilobyte.unapply)

      rw(Pairing(1, "omg"), "[1, \"omg\"]")
      rw(Box(1.02), "[\"1.02\"]")
      rw(Trilobyte(true, 3, (5, 6)), "[true, \"3.0\", [5, 6]]")
    }

    "sealedTraits" - {

      sealed trait T
      object T{
        case class A(i: Int) extends T
        case class B(s: String) extends T
        case object C extends T
      }

      def ARW = Case1ReadWriter(T.A.apply, T.A.unapply)
      def BRW = Case1ReadWriter(T.B.apply, T.B.unapply)

      val a: T = T.A(1)
      val b: T = T.B("omg")
      val c: T = T.C

      implicit def TWriter = new WriterCls[T](_ match{
        case T.A(i) => Js.Array(Seq(writeJs(0), writeJs(T.A(i))(ARW)))
        case T.B(s) => Js.Array(Seq(writeJs(1), writeJs(T.B(s))(BRW)))
        case T.C => Js.Array(Seq(writeJs(2)))
      })
      implicit def TReader = new ReaderCls[T](_ match{
        case Js.Array(Seq(Js.Number("0"), value)) => readJs(value)(ARW)
        case Js.Array(Seq(Js.Number("1"), value)) => readJs(value)(BRW)
        case Js.Array(Seq(Js.Number("2"))) => T.C
      })
      implicit def AWriter = new WriterCls[T.A](_ match{
        case T.A(i) => Js.Array(Seq(writeJs(0), writeJs(T.A(i))(ARW)))
      })
      implicit def AReader = new ReaderCls[T.A](_ match{
        case Js.Array(Seq(Js.Number("0"), value)) => readJs(value)(ARW)
      })
      implicit def BWriter = new WriterCls[T.B](_ match{
        case T.B(s) => Js.Array(Seq(writeJs(1), writeJs(T.B(s))(BRW)))
      })
      implicit def BReader = new ReaderCls[T.B](_ match{
        case Js.Array(Seq(Js.Number("1"), value)) => readJs(value)(BRW)
      })
      implicit def CWriter = new WriterCls[T.C.type](_ match{
        case T.C => Js.Array(Seq(writeJs(2)))
      })
      implicit def CReader = new ReaderCls[T.C.type](_ match{
        case Js.Array(Seq(Js.Number("2"))) => T.C
      })
      rw(a, "[0, [1]]")
      rw(T.A(2), "[0, [2]]")
      rw(b, "[1, [\"omg\"]]")
      rw(T.B("wtf"), "[1, [\"wtf\"]]")
      rw(c, "[2]")
      rw(T.C, "[2]")
    }

    "recursiveADT" - {

      sealed trait ConsList
      case class Cons(i: Int, next: ConsList) extends ConsList
      case object End extends ConsList

      implicit def ConsListWriter = new WriterCls[ConsList](_ match{
        case Cons(i, next) => Js.Array(Seq(writeJs(0), writeJs(Cons(i, next))(CRW)))
        case End => Js.Array(Seq(writeJs(1)))
      })
      implicit def ConsListReader = new ReaderCls[ConsList](_ match{
        case Js.Array(Seq(Js.Number("0"), value)) => readJs(value)(CRW)
        case Js.Array(Seq(Js.Number("1"))) => End
      })
      implicit def ConsWriter = new WriterCls[Cons](_ match{
        case Cons(i, next) => Js.Array(Seq(writeJs(0), writeJs(Cons(i, next))(CRW)))
      })
      implicit def ConsReader = new ReaderCls[Cons](_ match{
        case Js.Array(Seq(Js.Number("0"), value)) => readJs(value)(CRW)
      })
      def CRW: ReadWriter[Cons] = Case2ReadWriter(Cons.apply, Cons.unapply)
      val c = Cons(5, Cons(6, End))
      val cl: ConsList = c
      val serialized = "[0, [5, [0, [6, [1]]]]]"
      Seq(
        write(c) == serialized,
        c == read[ConsList](serialized),
        c == read[Cons](serialized),
        write(cl) == serialized,
        cl == read[ConsList](serialized),
        cl == read[Cons](serialized)
      ).foreach(x => assert(x))
    }
  }
}
