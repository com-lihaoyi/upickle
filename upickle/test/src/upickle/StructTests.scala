package upickle
import java.io.ByteArrayOutputStream
import utest._
import upickle.legacy.{read, write}
import upickle.core.compat._

import scala.concurrent.duration._
import TestUtil._

import java.util.UUID
import scala.reflect.ClassTag
import language.postfixOps

object StructTests extends TestSuite {

  val tests = Tests {
    test("arrays"){
      test("empty") - rwk(Array[Int](), "[]", upack.Arr())(_.toSeq)
      test("Boolean") - rwk(
        Array(true, false),
        "[true,false]",
        upack.Arr(upack.True, upack.False)
      )(_.toSeq)
      test("Int") - rwk(
        Array(1, 2, 3, 4, 5),
        "[1,2,3,4,5]",
        upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3), upack.Int32(4), upack.Int32(5))
      )(_.toSeq)
      test("String") - rwk(
        Array("omg", "i am", "cow"),
        """["omg","i am","cow"]""",
        upack.Arr(upack.Str("omg"), upack.Str("i am"), upack.Str("cow"))
      )(_.toSeq)
      test("Nulls") - rwk(
        Array(null, "i am", null),
         """[null,"i am",null]""",
        upack.Arr(upack.Null, upack.Str("i am"), upack.Null)
      )(_.toSeq)

    }

    test("tuples"){
      test("null") - rw(null: Tuple2[Int, Int], "null", upack.Null)
      test("2") - rw(
        (1, 2, 3.0),
        "[1,2,3]", "[1,2,3.0]",
        upack.Arr(upack.Int32(1), upack.Int32(2), upack.Float64(3.0))
      )
      test("2-1") - rw((false, 1), "[false,1]", upack.Arr(upack.False, upack.Int32(1)))
      test("3") - rw(
        ("omg", 1, "bbq"),
        """["omg",1,"bbq"]""",
        upack.Arr(upack.Str("omg"), upack.Int32(1), upack.Str("bbq"))
      )
      test("21") - rw(
        (1, 2.2, 3, 4, "5", 6, '7', 8, 9, 10.1, 11, 12, 13, 14.5, 15, "16", 17, 18, 19, 20, 21),
        """[1,2.2,3,4,"5",6,"7",8,9,10.1,11,12,13,14.5,15,"16",17,18,19,20,21]""",
        upack.Arr(
          upack.Int32(1),
          upack.Float64(2.2),
          upack.Int32(3),
          upack.Int32(4),
          upack.Str("5"),
          upack.Int32(6),
          upack.Int32('7'),
          upack.Int32(8),
          upack.Int32(9),
          upack.Float64(10.1),
          upack.Int32(11),
          upack.Int32(12),
          upack.Int32(13),
          upack.Float64(14.5),
          upack.Int32(15),
          upack.Str("16"),
          upack.Int32(17),
          upack.Int32(18),
          upack.Int32(19),
          upack.Int32(20),
          upack.Int32(21)
        )
      )
    }

    test("seqs"){
      test("Seq"){
        rw(Seq(true, false), "[true,false]", upack.Arr(upack.True, upack.False))
        rw(Seq(): Seq[Int], "[]", upack.Arr())
      }
      test("Vector"){
        rw(
          Vector(1, 2, 3, 4, 5),
          "[1,2,3,4,5]",
          upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3), upack.Int32(4), upack.Int32(5))
        )
        rw(Vector.empty[Int], "[]", upack.Arr())
      }
      test("List"){
        rw(
          List("omg", "i am", "cow"),
           """["omg","i am","cow"]""",
          upack.Arr(upack.Str("omg"), upack.Str("i am"), upack.Str("cow"))
        )
        rw(List(): List[String], "[]", upack.Arr())
        rw(Nil: List[List[Int]], "[]", upack.Arr())
      }
      test("Set") - rw(
        Set("omg", "i am", "cow"),
         """["omg","i am","cow"]""",
        upack.Arr(upack.Str("omg"), upack.Str("i am"), upack.Str("cow"))
      )
      test("SortedSet") - rw(
        collection.SortedSet("omg", "i am", "cow"),
         """["cow","i am","omg"]""",
        upack.Arr(upack.Str("cow"), upack.Str("i am"), upack.Str("omg"))
      )
      test("immutable"){
        test("Set") - rw(
          collection.immutable.Set("omg", "i am", "cow"),
           """["omg","i am","cow"]""",
          upack.Arr(upack.Str("omg"), upack.Str("i am"), upack.Str("cow"))
        )
        test("Seq") - rw(
          collection.immutable.Seq("omg", "i am", "cow"),
          """["omg","i am","cow"]""",
          upack.Arr(upack.Str("omg"), upack.Str("i am"), upack.Str("cow"))
        )
        test("List") - rw(
          collection.immutable.List("omg", "i am", "cow"),
           """["omg","i am","cow"]""",
          upack.Arr(upack.Str("omg"), upack.Str("i am"), upack.Str("cow"))
        )
        test("Queue") - rw(
          collection.immutable.Queue("omg", "i am", "cow"),
          """["omg","i am","cow"]""",
          upack.Arr(upack.Str("omg"), upack.Str("i am"), upack.Str("cow"))
        )
      }
      test("mutable"){
        test("Seq") - rw(
          collection.mutable.Seq("omg", "i am", "cow"),
           """["omg","i am","cow"]""",
          upack.Arr(upack.Str("omg"), upack.Str("i am"), upack.Str("cow"))
        )
        test("Buffer") - rw(
          collection.mutable.Buffer("omg", "i am", "cow"),
          """["omg","i am","cow"]""",
          upack.Arr(upack.Str("omg"), upack.Str("i am"), upack.Str("cow"))
        )
        test("SortedSet") - rw(
          collection.mutable.SortedSet("omg", "i am", "cow"),
          """["cow","i am","omg"]""",
          upack.Arr(upack.Str("cow"), upack.Str("i am"), upack.Str("omg"))
        )
      }
    }

    test("maps"){
      test("structured") {
        test("Structured") - rw[Map[List[Int], List[Int]]](
          Map(Nil -> List(1), List(1) -> List(1, 2, 3)),
          "[[[],[1]],[[1],[1,2,3]]]",
          upack.Obj(
            upack.Arr() -> upack.Arr(upack.Int32(1)),
            upack.Arr(upack.Int32(1)) -> upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3))
          ),
          upack.Arr(
            upack.Arr(upack.Arr(), upack.Arr(upack.Int32(1))),
            upack.Arr(upack.Arr(upack.Int32(1)), upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3)))
          )
        )
        test("Structured2") - rw[collection.mutable.Map[List[Int], List[Int]]](
          collection.mutable.Map(Nil -> List(1), List(1) -> List(1, 2, 3)),
          "[[[1],[1,2,3]], [[],[1]]]",
          "[[[],[1]],[[1],[1,2,3]]]",
          upack.Obj(
            upack.Arr() -> upack.Arr(upack.Int32(1)),
            upack.Arr(upack.Int32(1)) -> upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3))
          ),
          upack.Arr(
            upack.Arr(upack.Arr(), upack.Arr(upack.Int32(1))),
            upack.Arr(upack.Arr(upack.Int32(1)), upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3)))
          )
        )
        test("Structured3") - rw[collection.immutable.Map[List[Int], List[Int]]](
          collection.immutable.Map(Nil -> List(1), List(1) -> List(1, 2, 3)),
          "[[[],[1]],[[1],[1,2,3]]]",
          upack.Obj(
            upack.Arr() -> upack.Arr(upack.Int32(1)),
            upack.Arr(upack.Int32(1)) -> upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3))
          ),
          upack.Arr(
            upack.Arr(upack.Arr(), upack.Arr(upack.Int32(1))),
            upack.Arr(upack.Arr(upack.Int32(1)), upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3)))
          )
        )
        test("Structured4") - rw[collection.Map[List[Int], List[Int]]](
          collection.Map(Nil -> List(1), List(1) -> List(1, 2, 3)),
          "[[[],[1]],[[1],[1,2,3]]]",
          upack.Obj(
            upack.Arr() -> upack.Arr(upack.Int32(1)),
            upack.Arr(upack.Int32(1)) -> upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3))
          ),
          upack.Arr(
            upack.Arr(upack.Arr(), upack.Arr(upack.Int32(1))),
            upack.Arr(upack.Arr(upack.Int32(1)), upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3)))
          )
        )
        test("StructuredEmpty") - rw[Map[List[Int], List[Int]]](
          Map[List[Int], List[Int]](),
          "[]",
          upack.Obj(),
          upack.Arr()
        )
      }
      test("string"){
        test("String") - rw(
          Map("Hello" -> List(1), "World" -> List(1, 2, 3)),
          """{"Hello":[1],"World":[1,2,3]}""",
          """[["Hello",[1]],["World",[1,2,3]]]""",
          upack.Obj(
            upack.Str("Hello") -> upack.Arr(upack.Int32(1)),
            upack.Str("World") -> upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3))
          ),
          upack.Arr(
            upack.Arr(upack.Str("Hello"), upack.Arr(upack.Int32(1))),
            upack.Arr(upack.Str("World"), upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3)))
          )
        )
        test("String2") - rw(
          collection.Map("Hello" -> List(1), "World" -> List(1, 2, 3)),
          """{"Hello":[1],"World":[1,2,3]}""",
          """[["Hello",[1]],["World",[1,2,3]]]""",
          upack.Obj(
            upack.Str("Hello") -> upack.Arr(upack.Int32(1)),
            upack.Str("World") -> upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3))
          ),
          upack.Arr(
            upack.Arr(upack.Str("Hello"), upack.Arr(upack.Int32(1))),
            upack.Arr(upack.Str("World"), upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3)))
          )
        )
        test("String3") - rw(
          collection.immutable.Map("Hello" -> List(1), "World" -> List(1, 2, 3)),
          """{"Hello":[1],"World":[1,2,3]}""",
          """[["Hello",[1]],["World",[1,2,3]]]""",
          upack.Obj(
            upack.Str("Hello") -> upack.Arr(upack.Int32(1)),
            upack.Str("World") -> upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3))
          ),
          upack.Arr(
            upack.Arr(upack.Str("Hello"), upack.Arr(upack.Int32(1))),
            upack.Arr(upack.Str("World"), upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3)))
          )
        )
        test("String4") - rw[collection.mutable.Map[String, List[Int]]](
          collection.mutable.Map("Hello" -> List(1), "World" -> List(1, 2, 3)),
          """{"Hello":[1],"World":[1,2,3]}""",
          """[["Hello",[1]],["World",[1,2,3]]]""",
          upack.Obj(
            upack.Str("Hello") -> upack.Arr(upack.Int32(1)),
            upack.Str("World") -> upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3))
          ),
          upack.Arr(
            upack.Arr(upack.Str("Hello"), upack.Arr(upack.Int32(1))),
            upack.Arr(upack.Str("World"), upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3)))
          )
        )
        test("StringEmpty") - rw(
          Map[String, List[Int]](),
          "{}",
          "[]",
          upack.Obj(),
          upack.Arr()
        )
      }
      test("primitive"){
        test("ReadWriterJoin"){
          def foo[T: upickle.default.ReadWriter](x: T, expected: String) = {
            rw(Map(x -> x), expected)
          }

          foo[Int](123, """{"123": 123}""")
        }
        test("boolean") - rw(
          Map(true -> false, false -> true),
          """{"true": false, "false": true}""",
          """[[true, false], [false, true]]""",
          """[["true", false], ["false", true]]""",
          upack.Obj(upack.True -> upack.False, upack.False -> upack.True),
          upack.Arr(upack.Arr(upack.True, upack.False), upack.Arr(upack.False, upack.True)),
          upack.Arr(
            upack.Arr(upack.Str("true"), upack.False),
            upack.Arr(upack.Str("false"), upack.True)
          )
        )
        test("int") - rw(
          Map(1 -> 2, 3 -> 4, 5 -> 6),
          """{"1": 2, "3": 4, "5": 6}""",
          """[[1, 2], [3, 4], [5, 6]]""",
          """[["1", 2], ["3", 4], ["5", 6]]""",
          upack.Obj(
            upack.Int32(1) -> upack.Int32(2),
            upack.Int32(3) -> upack.Int32(4),
            upack.Int32(5) -> upack.Int32(6)
          ),
          upack.Arr(
            upack.Arr(upack.Int32(1), upack.Int32(2)),
            upack.Arr(upack.Int32(3), upack.Int32(4)),
            upack.Arr(upack.Int32(5), upack.Int32(6))
          ),
          upack.Arr(
            upack.Arr(upack.Str("1"), upack.Int32(2)),
            upack.Arr(upack.Str("3"), upack.Int32(4)),
            upack.Arr(upack.Str("5"), upack.Int32(6))
          )
        )
        test("long") - rw(
          Map(1L -> 2L, 3L -> 4L, 5L -> 6L),
          """{"1": 2, "3": 4, "5": 6}""",
          """[[1, 2], [3, 4], [5, 6]]""",
          """[["1", 2], ["3", 4], ["5", 6]]""",
          upack.Obj(
            upack.Int64(1) -> upack.Int64(2),
            upack.Int64(3) -> upack.Int64(4),
            upack.Int64(5) -> upack.Int64(6)
          ),
          upack.Arr(
            upack.Arr(upack.Int64(1), upack.Int64(2)),
            upack.Arr(upack.Int64(3), upack.Int64(4)),
            upack.Arr(upack.Int64(5), upack.Int64(6))
          ),
          upack.Arr(
            upack.Arr(upack.Str("1"), upack.Int64(2)),
            upack.Arr(upack.Str("3"), upack.Int64(4)),
            upack.Arr(upack.Str("5"), upack.Int64(6))
          )
        )

        test("char") - rw(
          Map('a' -> 'b', 'c' -> 'd', 'e' -> 'f'),
          """{"a": "b", "c": "d", "e": "f"}""",
          """[["a", "b"], ["c", "d"], ["e", "f"]]""",
          upack.Obj(
            upack.Int32('a') -> upack.Int32('b'),
            upack.Int32('c') -> upack.Int32('d'),
            upack.Int32('e') -> upack.Int32('f')
          ),
          upack.Arr(
            upack.Arr(upack.Int32('a'), upack.Int32('b')),
            upack.Arr(upack.Int32('c'), upack.Int32('d')),
            upack.Arr(upack.Int32('e'), upack.Int32('f'))
          ),
          upack.Arr(
            upack.Arr(upack.Str("a"), upack.Int32('b')),
            upack.Arr(upack.Str("c"), upack.Int32('d')),
            upack.Arr(upack.Str("e"), upack.Int32('f'))
          )
        )

        test("uuid") - rw(
          Map(
            new java.util.UUID(123456789L, 987654321L) ->
            new java.util.UUID(987654321L, 123456789L)
          ),
          """{"00000000-075b-cd15-0000-00003ade68b1": "00000000-3ade-68b1-0000-0000075bcd15"}""",
          """[["00000000-075b-cd15-0000-00003ade68b1", "00000000-3ade-68b1-0000-0000075bcd15"]]""",
          upack.Obj(
            upack.Str("00000000-075b-cd15-0000-00003ade68b1") ->
            upack.Str("00000000-3ade-68b1-0000-0000075bcd15")
          ),
          upack.Arr(
            upack.Arr(
              upack.Str("00000000-075b-cd15-0000-00003ade68b1"),
              upack.Str("00000000-3ade-68b1-0000-0000075bcd15")
            )
          )
        )

        test("symbol") - rw(
          Map(Symbol("abc") -> Symbol("def")),
          """{"abc": "def"}""",
          """[["abc", "def"]]""",
          upack.Obj(upack.Str("abc") -> upack.Str("def")),
          upack.Arr(upack.Arr(upack.Str("abc"), upack.Str("def")))
        )
      }

      test("variants"){
        test("LinkedHashMap") - rw(
          scala.collection.mutable.LinkedHashMap("Hello" -> List(1), "World" -> List(1, 2, 3)),
          """{"Hello":[1],"World":[1,2,3]}""",
          """[["Hello",[1]],["World",[1,2,3]]]""",
          upack.Obj(
            upack.Str("Hello") -> upack.Arr(upack.Int32(1)),
            upack.Str("World") -> upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3))
          ),
          upack.Arr(
            upack.Arr(upack.Str("Hello"), upack.Arr(upack.Int32(1))),
            upack.Arr(upack.Str("World"), upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3)))
          )
        )
        test("SortedMap"){
          test("immutable") - rw(
            scala.collection.immutable.SortedMap("Hello" -> List(1), "World" -> List(1, 2, 3)),
            """{"Hello":[1],"World":[1,2,3]}""",
            """[["Hello",[1]],["World",[1,2,3]]]""",
            upack.Obj(
              upack.Str("Hello") -> upack.Arr(upack.Int32(1)),
              upack.Str("World") -> upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3))
            ),
            upack.Arr(
              upack.Arr(upack.Str("Hello"), upack.Arr(upack.Int32(1))),
              upack.Arr(upack.Str("World"), upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3)))
            )
          )
          test("mutable") - rw(
            scala.collection.mutable.SortedMap("Hello" -> List(1), "World" -> List(1, 2, 3)),
            """{"Hello":[1],"World":[1,2,3]}""",
            """[["Hello",[1]],["World",[1,2,3]]]""",
            upack.Obj(
              upack.Str("Hello") -> upack.Arr(upack.Int32(1)),
              upack.Str("World") -> upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3))
            ),
            upack.Arr(
              upack.Arr(upack.Str("Hello"), upack.Arr(upack.Int32(1))),
              upack.Arr(upack.Str("World"), upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3)))
            )
          )
          test("collection") - rw(
            scala.collection.SortedMap("Hello" -> List(1), "World" -> List(1, 2, 3)),
            """{"Hello":[1],"World":[1,2,3]}""",
            """[["Hello",[1]],["World",[1,2,3]]]""",
            upack.Obj(
              upack.Str("Hello") -> upack.Arr(upack.Int32(1)),
              upack.Str("World") -> upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3))
            ),
            upack.Arr(
              upack.Arr(upack.Str("Hello"), upack.Arr(upack.Int32(1))),
              upack.Arr(upack.Str("World"), upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3)))
            )
          )
        }
      }
    }

    test("option"){
      test("Some") - rw(Some(123), "123", upack.Int32(123))
      test("None") - rw(None, "null", upack.Null)
      test("Option"){
        rw(Some(123): Option[Int], "123", upack.Int32(123))
        rw(None: Option[Int], "null", upack.Null)
      }
    }

    test("either"){
      test("Left") - rw(
        Left(123): Left[Int, Int],
        """[0,123]""",
        upack.Arr(upack.Float64(0), upack.Int32(123))
      )
      test("Right") - rw(
        Right(123): Right[Int, Int],
        """[1,123]""",
        upack.Arr(upack.Float64(1), upack.Int32(123))
      )
      test("Either"){
        rw(
          Left(123): Either[Int, Int],
          """[0,123]""",
          upack.Arr(upack.Float64(0), upack.Int32(123))
        )
        rw(
          Right(123): Either[Int, Int],
          """[1,123]""",
          upack.Arr(upack.Float64(1), upack.Int32(123))
        )
      }
    }

    test("combinations"){
      test("SeqListMapOptionString") - rw[Seq[List[Map[Option[String], String]]]](
        Seq(Nil, List(Map(Some("omg") -> "omg"), Map(Some("lol") -> "lol", None -> "")), List(Map())),
        """[[],[[["omg","omg"]],[["lol","lol"],[null,""]]],[[]]]""",
        upack.Arr(
          upack.Arr(),
          upack.Arr(
            upack.Obj(upack.Str("omg") -> upack.Str("omg")),
            upack.Obj(
              upack.Str("lol") -> upack.Str("lol"),
              upack.Null -> upack.Str("")
            )
          ),
          upack.Arr(upack.Obj())
        )
      )

//      test("NullySeqListMapOptionString") - rw[Seq[List[Map[Option[String], String]]]](
//        Seq(Nil, List(Map(Some(null) -> "omg"), Map(Some("lol") -> null, None -> "")), List(null)),
//        """[[],[[[[null],"omg"]],[[["lol"],null],[[],""]]],[null]]""",
//        upack.Arr(
//          upack.Arr(),
//          upack.Arr(
//            upack.Obj(upack.Arr(upack.Null) -> upack.Str("omg")),
//            upack.Obj(
//              upack.Arr(upack.Str("lol")) -> upack.Null,
//              upack.Arr() -> upack.Str("")
//            )
//          ),
//          upack.Arr(upack.Null)
//        )
//      )

      test("tuples") - rw(
        (1, (2.0, true), (3.0, 4.0, 5.0)),
        """[1,[2,true],[3,4,5]]""",
        """[1,[2.0,true],[3.0,4.0,5.0]]""",
        upack.Arr(
          upack.Int32(1),
          upack.Arr(upack.Float64(2.0), upack.True),
          upack.Arr(upack.Float64(3.0), upack.Float64(4.0), upack.Float64(5.0))
        )
      )

      test("EitherDurationOptionDuration"){
        rw(
          Left(10 seconds): Either[Duration, Int],
          """[0,"10000000000"]""",
          upack.Arr(upack.Float64(0.0), upack.Str("10000000000"))
        )
        rw(
          Right(Some(0.33 millis)): Either[Int, Option[Duration]],
          """[1,"330000"]""",
          upack.Arr(upack.Float64(1.0), upack.Str("330000"))
        )
        rw(
          Left(10 seconds): Either[Duration, Option[Duration]],
          """[0,"10000000000"]""",
          upack.Arr(upack.Float64(0.0), upack.Str("10000000000"))
        )
        rw(
          Right(Some(0.33 millis)): Either[Duration, Option[Duration]],
          """[1,"330000"]""",
          upack.Arr(upack.Float64(1.0), upack.Str("330000"))
        )
      }
    }

    test("writeBytesTo"){
      test("json") {
        type Thing = Seq[List[Map[Option[String], String]]]
        val thing: Thing = Seq(Nil, List(Map(Some("omg") -> "omg"), Map(Some("lol") -> "lol", None -> "")), List(Map()))
        val out = new ByteArrayOutputStream()
        upickle.default.stream(thing).writeBytesTo(out)
        out.toByteArray ==> upickle.default.write(thing).getBytes
      }
      test("msgpack") {
        type Thing = Seq[List[Map[Option[String], String]]]
        val thing: Thing = Seq(Nil, List(Map(Some("omg") -> "omg"), Map(Some("lol") -> "lol", None -> "")), List(Map()))
        val out = new ByteArrayOutputStream()
        upickle.default.streamBinary(thing).writeBytesTo(out)
        out.toByteArray ==> upickle.default.writeBinary(thing)
      }
    }

    test("transmutation"){
      test("vectorToList"){
        val vectorToList = read[List[Double]](write(Vector(1.1, 2.2, 3.3)))
        assert(
          vectorToList.isInstanceOf[List[Double]],
          vectorToList == List(1.1, 2.2, 3.3)
        )

      }
      test("listToMap"){
        val listToMap = read[Map[Int, String]](write(List((1, "1"), (2, "2"))))
        assert(
          listToMap.isInstanceOf[Map[Int, String]],
          listToMap == Map(1 -> "1", 2 -> "2")
        )
      }
    }

    test("extra"){
      val uuidString = "01020304-0506-0708-0901-020304050607"
      val uuid = UUID.fromString(uuidString)
      test("UUID"){
        rw(uuid, s""" "$uuidString" """)
      }
    }

    test("jsValue"){
      test("value"){
        val value:ujson.Value = ujson.Str("test")
        rw(value, """ "test" """.trim)
      }
      test("str") - rw(ujson.Str("test"), """"test"""")
      test("num") - rw(ujson.Num(7), """7""")
      test("obj"){
        test("nested") - rw(ujson.Obj("foo" -> ujson.Null, "bar" -> ujson.Obj("baz" -> ujson.Str("str"))), """{"foo":null,"bar":{"baz":"str"}}""")
        test("empty") - rw(ujson.Obj(), """{}""")
      }
      test("arr"){
        test("nonEmpty") - rw(ujson.Arr(ujson.Num(5), ujson.Num(6)), """[5,6]""")
        test("empty") - rw(ujson.Arr(), """[]""")
      }
      test("true") - rw(ujson.True, """true""")
      test("true") - rw(ujson.Bool(true), """true""")
      test("false") - rw(ujson.False, """false""")
      test("false") - rw(ujson.Bool(false), """false""")
      test("null") - rw(ujson.Null, """null""")
    }
    test("sortKeys") {
      test("streaming") {
        val raw = """{"d": [{"c": 0, "b": 1}], "a": []}"""
        val sorted =
          """{
            |    "a": [],
            |    "d": [
            |        {
            |            "b": 1,
            |            "c": 0
            |        }
            |    ]
            |}""".stripMargin
        val struct = upickle.default.read[Map[String, Seq[Map[String, Int]]]](raw)

        upickle.default.write(struct, indent = 4, sortKeys = true) ==> sorted

        val baos = new java.io.ByteArrayOutputStream
        upickle.default.writeToOutputStream(struct, baos, indent = 4, sortKeys = true)
        baos.toString ==> sorted

        val writer = new java.io.StringWriter
        upickle.default.writeTo(struct, writer, indent = 4, sortKeys = true)
        writer.toString ==> sorted

        new String(upickle.default.writeToByteArray(struct, indent = 4, sortKeys = true)) ==> sorted

        val baos2 = new java.io.ByteArrayOutputStream
        upickle.default.stream(struct, indent = 4, sortKeys = true).writeBytesTo(baos2)
        baos2.toString() ==> sorted
      }

      test("ints") {
        val raw = """{"27": [{"10": 0, "2": 1}], "3": []}"""
        val sorted =
          """{
            |    "3": [],
            |    "27": [
            |        {
            |            "2": 1,
            |            "10": 0
            |        }
            |    ]
            |}""".stripMargin
        val struct = upickle.default.read[Map[Int, Seq[Map[Int, Int]]]](raw)

        upickle.default.write(struct, indent = 4, sortKeys = true) ==> sorted
      }
      test("longs") {
        val raw = """{"27": [{"10": 0, "2": 1}], "300": []}"""
        val sorted =
          """{
            |    "27": [
            |        {
            |            "2": 1,
            |            "10": 0
            |        }
            |    ],
            |    "300": []
            |}""".stripMargin
        val struct = upickle.default.read[Map[Long, Seq[Map[Long, Int]]]](raw)

        upickle.default.write(struct, indent = 4, sortKeys = true) ==> sorted
      }
      test("floats") {
        val raw = """{"27.5": [{"10.5": 0, "2.5": 1}], "3.5": []}"""
        val sorted =
          """{
            |    "3.5": [],
            |    "27.5": [
            |        {
            |            "2.5": 1,
            |            "10.5": 0
            |        }
            |    ]
            |}""".stripMargin
        val struct = upickle.default.read[Map[Float, Seq[Map[Float, Int]]]](raw)

        upickle.default.write(struct, indent = 4, sortKeys = true) ==> sorted
      }
      test("doubles") {
        val raw = """{"27.5": [{"10.5": 0, "2.5": 1}], "3.5": []}"""
        val sorted =
          """{
            |    "3.5": [],
            |    "27.5": [
            |        {
            |            "2.5": 1,
            |            "10.5": 0
            |        }
            |    ]
            |}""".stripMargin
        val struct = upickle.default.read[Map[Double, Seq[Map[Double, Int]]]](raw)

        upickle.default.write(struct, indent = 4, sortKeys = true) ==> sorted
      }
      test("strings") {
        // Make sure that when we treat things as Strings, they are sorted
        // as strings, unlike the above cases where they are treated as numbers
        val raw = """{"27.5": [{"10.5": 0, "2.5": 1}], "3.5": []}"""
        val sorted =
          """{
            |    "27.5": [
            |        {
            |            "10.5": 0,
            |            "2.5": 1
            |        }
            |    ],
            |    "3.5": []
            |}""".stripMargin
        val struct = upickle.default.read[Map[String, Seq[Map[String, Int]]]](raw)

        upickle.default.write(struct, indent = 4, sortKeys = true) ==> sorted
      }
    }
  }
}

