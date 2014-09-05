package upickle
import utest._
import scala.concurrent.duration._
import TestUtil._

import scala.reflect.ClassTag


object StructTests extends TestSuite{
  Seq(1).to[Vector]
  val tests = TestSuite{
    'arrays{
      'empty-rwk(Array[Int](), "[]")(_.toSeq)
      'Boolean-rwk(Array(true, false), "[true,false]")(_.toSeq)
      'Int-rwk(Array(1, 2, 3, 4, 5), "[1,2,3,4,5]")(_.toSeq)
      'String-rwk(Array("omg", "i am", "cow"), """["omg","i am","cow"]""")(_.toSeq)
      'Nulls-rwk(Array(null, "i am", null), """[null,"i am",null]""")(_.toSeq)

    }

    'tuples{
      'null-rw(null: Tuple2[Int, Int], "null")
      "2" - rw((1, 2, 3.0), "[1,2,3.0]", "[1,2,3]")
      "2-1" - rw((false, 1), "[false,1]")
      "3" - rw(("omg", 1, "bbq"), """["omg",1,"bbq"]""")
      "21" - rw(
        (1, 2.2, 3, 4, "5", 6, '7', 8, 9, 10.1, 11, 12, 13, 14.5, 15, "16", 17, 18, 19, 20, 21),
        """[1,2.2,3,4,"5",6,"7",8,9,10.1,11,12,13,14.5,15,"16",17,18,19,20,21]"""
      )
    }

    'seqs{
      'Seq{
        rw(Seq(true, false), "[true,false]")
        rw(Seq(): Seq[Int], "[]")
      }
      'Vector{
        rw(Vector(1, 2, 3, 4, 5), "[1,2,3,4,5]")
        rw(Vector.empty[Int], "[]")
      }
      'List{
        rw(List("omg", "i am", "cow"), """["omg","i am","cow"]""")
        rw(List(): List[String], "[]")
        rw(Nil: List[List[Int]], "[]")
      }
      'Set-rw(Set("omg", "i am", "cow"), """["omg","i am","cow"]""")
      'SortedSet-rw(collection.SortedSet("omg", "i am", "cow"), """["cow","i am","omg"]""")
      'immutable {
        'Set - rw(collection.immutable.Set("omg", "i am", "cow"), """["omg","i am","cow"]""")
        'Seq - rw(collection.immutable.Seq("omg", "i am", "cow"), """["omg","i am","cow"]""")
        'List - rw(collection.immutable.List("omg", "i am", "cow"), """["omg","i am","cow"]""")
        'Queue - rw(collection.immutable.Queue("omg", "i am", "cow"), """["omg","i am","cow"]""")
      }
      'mutable {
        'Seq - rw(collection.mutable.Seq("omg", "i am", "cow"), """["omg","i am","cow"]""")
        'Buffer - rw(collection.mutable.Buffer("omg", "i am", "cow"), """["omg","i am","cow"]""")
        'SortedSet - rw(collection.mutable.SortedSet("omg", "i am", "cow"), """["cow","i am","omg"]""")
        'LinkedList - rw(collection.mutable.LinkedList("omg", "i am", "cow"), """["omg","i am","cow"]""")
      }
      'Map-rw(Map(Nil -> List(1), List(1) -> List(1, 2, 3)), "[[[],[1]],[[1],[1,2,3]]]")
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
      'Left-rw(Left(123): Left[Int, Int], """[0,123]""")
      'Right-rw(Right(123): Right[Int, Int], """[1,123]""")
      'Either{
        rw(Left(123): Either[Int, Int], """[0,123]""")
        rw(Right(123): Either[Int, Int], """[1,123]""")
      }
    }

    'durations{
      'inf-rw(Duration.Inf, """ "inf" """)
      "-inf" - rw(Duration.MinusInf, """ "-inf" """)
      'undef-rw(Duration.Undefined, """ "undef" """)
      "1-second" - rw(1.second, """ "1000000000" """)
      "2-hour" - rw(2.hours, """ "7200000000000" """)
    }

    'combinations{
      'SeqListMapOptionString-rw[Seq[List[Map[Option[String], String]]]](
        Seq(Nil, List(Map(Some("omg") -> "omg"), Map(Some("lol") -> "lol", None -> "")), List(Map())),
        """[[],[[[["omg"],"omg"]],[[["lol"],"lol"],[[],""]]],[[]]]"""
      )

      'NullySeqListMapOptionString-rw[Seq[List[Map[Option[String], String]]]](
        Seq(Nil, List(Map(Some(null) -> "omg"), Map(Some("lol") -> null, None -> "")), List(null)),
        """[[],[[[[null],"omg"]],[[["lol"],null],[[],""]]],[null]]"""
      )

      'tuples-rw(
        (1, (2.0, true), (3.0, 4.0, 5.0)),
        """[1,[2.0,true],[3.0,4.0,5.0]]""",
        """[1,[2,true],[3,4,5]]"""
      )

      'EitherDurationOptionDuration{
        rw(Left(10 seconds): Either[Duration, Int], """[0,"10000000000"]""")
        rw(Right(Some(0.33 millis)): Either[Int, Option[Duration]], """[1,["330000"]]""")
        rw(Left(10 seconds): Either[Duration, Option[Duration]], """[0,"10000000000"]""")
        rw(Right(Some(0.33 millis)): Either[Duration, Option[Duration]], """[1,["330000"]]""")
      }
    }

    'transmutation{
      'vectorToList{
        val vectorToList = read[Seq[Double]](write(Vector(1.1, 2.2, 3.3)))
        assert(
          vectorToList.isInstanceOf[Vector[Double]],
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
  }
}
