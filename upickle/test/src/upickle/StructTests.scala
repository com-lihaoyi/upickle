package upickle
import utest._
import upickle.legacy.{read, write}

import scala.collection.compat._
import scala.concurrent.duration._
import TestUtil._
import java.util.UUID

import acyclic.file

import scala.reflect.ClassTag
import language.postfixOps

object StructTests extends TestSuite {
  Seq(1).to(Vector)
  val tests = Tests {
    test("arrays"){
      test("empty") - rwk(Array[Int](), "[]")(_.toSeq)
      test("Boolean") - rwk(Array(true, false), "[true,false]")(_.toSeq)
      test("Int") - rwk(Array(1, 2, 3, 4, 5), "[1,2,3,4,5]")(_.toSeq)
      test("String") - rwk(Array("omg", "i am", "cow"), """["omg","i am","cow"]""")(_.toSeq)
      test("Nulls") - rwk(Array(null, "i am", null), """[null,"i am",null]""")(_.toSeq)

    }

    test("tuples"){
      test("null") - rw(null: Tuple2[Int, Int], "null")
      "2" - rw((1, 2, 3.0), "[1,2,3.0]", "[1,2,3]")
      "2-1" - rw((false, 1), "[false,1]")
      "3" - rw(("omg", 1, "bbq"), """["omg",1,"bbq"]""")
      "21" - rw(
        (1, 2.2, 3, 4, "5", 6, '7', 8, 9, 10.1, 11, 12, 13, 14.5, 15, "16", 17, 18, 19, 20, 21),
        """[1,2.2,3,4,"5",6,"7",8,9,10.1,11,12,13,14.5,15,"16",17,18,19,20,21]"""
      )
    }

    test("seqs"){
      test("Seq"){
        rw(Seq(true, false), "[true,false]")
        rw(Seq(): Seq[Int], "[]")
      }
      test("Vector"){
        rw(Vector(1, 2, 3, 4, 5), "[1,2,3,4,5]")
        rw(Vector.empty[Int], "[]")
      }
      test("List"){
        rw(List("omg", "i am", "cow"), """["omg","i am","cow"]""")
        rw(List(): List[String], "[]")
        rw(Nil: List[List[Int]], "[]")
      }
      test("Set") - rw(Set("omg", "i am", "cow"), """["omg","i am","cow"]""")
      test("SortedSet") - rw(collection.SortedSet("omg", "i am", "cow"), """["cow","i am","omg"]""")
      test("immutable"){
        test("Set") - rw(collection.immutable.Set("omg", "i am", "cow"), """["omg","i am","cow"]""")
        test("Seq") - rw(collection.immutable.Seq("omg", "i am", "cow"), """["omg","i am","cow"]""")
        test("List") - rw(collection.immutable.List("omg", "i am", "cow"), """["omg","i am","cow"]""")
        test("Queue") - rw(collection.immutable.Queue("omg", "i am", "cow"), """["omg","i am","cow"]""")
      }
      test("mutable"){
        test("Seq") - rw(collection.mutable.Seq("omg", "i am", "cow"), """["omg","i am","cow"]""")
        test("Buffer") - rw(collection.mutable.Buffer("omg", "i am", "cow"), """["omg","i am","cow"]""")
        test("SortedSet") - rw(collection.mutable.SortedSet("omg", "i am", "cow"), """["cow","i am","omg"]""")
      }
      test("Map"){
        test("Structured") - rw(
          Map(Nil -> List(1), List(1) -> List(1, 2, 3)),
          "[[[],[1]],[[1],[1,2,3]]]"
        )
        test("Structured2") - rw(
          collection.mutable.Map(Nil -> List(1), List(1) -> List(1, 2, 3)),
          "[[[],[1]],[[1],[1,2,3]]]"
        )
        test("Structured3") - rw(
          collection.immutable.Map(Nil -> List(1), List(1) -> List(1, 2, 3)),
          "[[[],[1]],[[1],[1,2,3]]]"
        )
        test("Structured4") - rw(
          collection.Map(Nil -> List(1), List(1) -> List(1, 2, 3)),
          "[[[],[1]],[[1],[1,2,3]]]"
        )
        test("StructuredEmpty") - rw(
          Map[List[Int], List[Int]](),
          "[]"
        )
        test("String") - rw(
          Map("Hello" -> List(1), "World" -> List(1, 2, 3)),
          """{"Hello":[1],"World":[1,2,3]}"""
        )
        test("String2") - rw(
          collection.Map("Hello" -> List(1), "World" -> List(1, 2, 3)),
          """{"Hello":[1],"World":[1,2,3]}"""
        )
        test("String3") - rw(
          collection.immutable.Map("Hello" -> List(1), "World" -> List(1, 2, 3)),
          """{"Hello":[1],"World":[1,2,3]}"""
        )
        test("String4") - rw(
          collection.mutable.Map("Hello" -> List(1), "World" -> List(1, 2, 3)),
          """{"Hello":[1],"World":[1,2,3]}"""
        )
        test("StringEmpty") - rw(
          Map[String, List[Int]](),
          "{}"
        )
      }
    }

    test("option"){
      test("Some") - rw(Some(123), "[123]")
      test("None") - rw(None, "[]")
      test("Option"){
        rw(Some(123): Option[Int], "[123]")
        rw(None: Option[Int], "[]")
      }
    }

    test("either"){
      test("Left") - rw(Left(123): Left[Int, Int], """[0,123]""")
      test("Right") - rw(Right(123): Right[Int, Int], """[1,123]""")
      test("Either"){
        rw(Left(123): Either[Int, Int], """[0,123]""")
        rw(Right(123): Either[Int, Int], """[1,123]""")
      }
    }

    test("durations"){
      test("inf") - rw(Duration.Inf, """ "inf" """)
      "-inf" - rw(Duration.MinusInf, """ "-inf" """)
      test("undef") - rw(Duration.Undefined, """ "undef" """)
      "1-second" - rw(1.second, """ "1000000000" """)
      "2-hour" - rw(2.hours, """ "7200000000000" """)
    }

    test("combinations"){
      test("SeqListMapOptionString") - rw[Seq[List[Map[Option[String], String]]]](
        Seq(Nil, List(Map(Some("omg") -> "omg"), Map(Some("lol") -> "lol", None -> "")), List(Map())),
        """[[],[[[["omg"],"omg"]],[[["lol"],"lol"],[[],""]]],[[]]]"""
      )

      test("NullySeqListMapOptionString") - rw[Seq[List[Map[Option[String], String]]]](
        Seq(Nil, List(Map(Some(null) -> "omg"), Map(Some("lol") -> null, None -> "")), List(null)),
        """[[],[[[[null],"omg"]],[[["lol"],null],[[],""]]],[null]]"""
      )

      test("tuples") - rw(
        (1, (2.0, true), (3.0, 4.0, 5.0)),
        """[1,[2.0,true],[3.0,4.0,5.0]]""",
        """[1,[2,true],[3,4,5]]"""
      )

      test("EitherDurationOptionDuration"){
        rw(Left(10 seconds): Either[Duration, Int], """[0,"10000000000"]""")
        rw(Right(Some(0.33 millis)): Either[Int, Option[Duration]], """[1,["330000"]]""")
        rw(Left(10 seconds): Either[Duration, Option[Duration]], """[0,"10000000000"]""")
        rw(Right(Some(0.33 millis)): Either[Duration, Option[Duration]], """[1,["330000"]]""")
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
  }
}

