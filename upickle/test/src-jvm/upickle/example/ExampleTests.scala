package upickle.example

import java.io.StringWriter

import acyclic.file
import upickle.{TestUtil, default}
import utest._
import upickle.default.{macroRW, ReadWriter => RW}
import ujson.{IncompleteParseException, NoOpVisitor, ParseException, Transformable}
import ujson.{Js, BytesRenderer, StringRenderer}
object Simple {
  case class Thing(myFieldA: Int, myFieldB: String)
  object Thing{
    implicit def rw: RW[Thing] = macroRW
  }
  case class Big(i: Int, b: Boolean, str: String, c: Char, t: Thing)
  object Big{
    implicit def rw: RW[Big] = macroRW
  }
}
object Sealed{
  sealed trait IntOrTuple
  object IntOrTuple{
    implicit def rw: RW[IntOrTuple] = RW.merge(IntThing.rw, TupleThing.rw)
  }
  case class IntThing(i: Int) extends IntOrTuple
  object IntThing{
    implicit def rw: RW[IntThing] = macroRW
  }
  case class TupleThing(name: String, t: (Int, Int)) extends IntOrTuple
  object TupleThing{
    implicit def rw: RW[TupleThing] = macroRW
  }
}
object Recursive{
  case class Foo(i: Int)
  object Foo{
    implicit def rw: RW[Foo] = macroRW
  }
  case class Bar(name: String, foos: Seq[Foo])
  object Bar{
    implicit def rw: RW[Bar] = macroRW
  }
}
object Defaults{
  case class FooDefault(i: Int = 10, s: String = "lol")
  object FooDefault{
    implicit def rw: RW[FooDefault] = macroRW
  }
}
object Keyed{
  case class KeyBar(@upickle.key("hehehe") kekeke: Int)
  object KeyBar{
    implicit def rw: RW[KeyBar] = macroRW
  }
}
object KeyedTag{
  sealed trait A
  object A{
    implicit def rw: RW[A] = RW.merge(B.rw, macroRW[C.type])
  }
  @upickle.key("Bee") case class B(i: Int) extends A
  object B{
    implicit def rw: RW[B] = macroRW
  }
  case object C extends A
}
//object Custom2{
//  import upickle.Js
//  class CustomThing2(val i: Int, val s: String)
//  object CustomThing2{
//    implicit val thing2Writer = upickle.default.Writer[CustomThing2]{
//      case t => Js.Str(t.i + " " + t.s)
//    }
//    implicit val thing2Reader = upickle.default.Reader[CustomThing2]{
//      case Js.Str(str) =>
//        val Array(i, s) = str.toString.split(" ")
//        new CustomThing2(i.toInt, s)
//    }
//  }
//}

import KeyedTag._
import Keyed._
import Sealed._
import Simple._
import Recursive._
import Defaults._

object ExampleTests extends TestSuite {

  import TestUtil._
  val tests = Tests {
    'simple{
      import upickle.default._

      write(1)                          ==> "1"

      write(Seq(1, 2, 3))               ==> "[1,2,3]"

      read[Seq[Int]]("[1,2,3]")       ==> List(1, 2, 3)

      write((1, "omg", true))           ==> """[1,"omg",true]"""

      type Tup = (Int, String, Boolean)

      read[Tup]("""[1,"omg",true]""") ==> (1, "omg", true)
    }
    'more{
      import upickle.default._
      'booleans{
        write(true: Boolean)              ==> "true"
        write(false: Boolean)             ==> "false"
      }
      'numbers{
        write(12: Int)                    ==> "12"
        write(12: Short)                  ==> "12"
        write(12: Byte)                   ==> "12"
        write(Int.MaxValue)               ==> "2147483647"
        write(Int.MinValue)               ==> "-2147483648"
        write(12.5f: Float)               ==> "12.5"
        write(12.5: Double)               ==> "12.5"
      }
      'longs{
        write(12: Long)                   ==> "\"12\""
        write(4000000000000L: Long)       ==> "\"4000000000000\""
      }
      'specialNumbers{
        write(1.0/0: Double)              ==> "\"Infinity\""
        write(Float.PositiveInfinity)     ==> "\"Infinity\""
        write(Float.NegativeInfinity)     ==> "\"-Infinity\""
      }
      'charStrings{
        write('o')                        ==> "\"o\""
        write("omg")                      ==> "\"omg\""
      }
      'seqs{
        write(Array(1, 2, 3))             ==> "[1,2,3]"

        // You can pass in an `indent` parameter to format it nicely
        write(Array(1, 2, 3), indent = 4)  ==>
          """[
            |    1,
            |    2,
            |    3
            |]""".stripMargin

        write(Seq(1, 2, 3))               ==> "[1,2,3]"
        write(Vector(1, 2, 3))            ==> "[1,2,3]"
        write(List(1, 2, 3))              ==> "[1,2,3]"
        import collection.immutable.SortedSet
        write(SortedSet(1, 2, 3))         ==> "[1,2,3]"
      }
      'options{
        write(Some(1))                    ==> "[1]"
        write(None)                       ==> "[]"
      }
      'tuples{
        write((1, "omg"))                 ==> """[1,"omg"]"""
        write((1, "omg", true))           ==> """[1,"omg",true]"""
      }

      'caseClass{
        import upickle._
        write(Thing(1, "gg"))             ==> """{"myFieldA":1,"myFieldB":"gg"}"""
        read[Thing]("""{"myFieldA":1,"myFieldB":"gg"}""") ==> Thing(1, "gg")
        write(Big(1, true, "lol", 'Z', Thing(7, ""))) ==>
          """{"i":1,"b":true,"str":"lol","c":"Z","t":{"myFieldA":7,"myFieldB":""}}"""

        write(Big(1, true, "lol", 'Z', Thing(7, "")), indent = 4) ==>
          """{
            |    "i": 1,
            |    "b": true,
            |    "str": "lol",
            |    "c": "Z",
            |    "t": {
            |        "myFieldA": 7,
            |        "myFieldB": ""
            |    }
            |}""".stripMargin
        }


      'sealed{
        write(IntThing(1)) ==> """{"$type":"upickle.example.Sealed.IntThing","i":1}"""

        write(TupleThing("naeem", (1, 2))) ==>
          """{"$type":"upickle.example.Sealed.TupleThing","name":"naeem","t":[1,2]}"""

        // You can read tagged value without knowing its
        // type in advance, just use type of the sealed trait
        read[IntOrTuple]("""{"$type":"upickle.example.Sealed.IntThing","i":1}""") ==> IntThing(1)

      }
      'recursive{
        write((((1, 2), (3, 4)), ((5, 6), (7, 8)))) ==>
          """[[[1,2],[3,4]],[[5,6],[7,8]]]"""

        write(Seq(Thing(1, "g"), Thing(2, "k"))) ==>
          """[{"myFieldA":1,"myFieldB":"g"},{"myFieldA":2,"myFieldB":"k"}]"""

        write(Bar("bearrr", Seq(Foo(1), Foo(2), Foo(3)))) ==>
          """{"name":"bearrr","foos":[{"i":1},{"i":2},{"i":3}]}"""

      }
      'null{
        write(Bar(null, Seq(Foo(1), null, Foo(3)))) ==>
          """{"name":null,"foos":[{"i":1},null,{"i":3}]}"""
      }
    }
    'defaults{
      import upickle.default._
      'reading{

        read[FooDefault]("{}")                ==> FooDefault(10, "lol")
        read[FooDefault]("""{"i": 123}""")    ==> FooDefault(123,"lol")
      }
      'writing{
        write(FooDefault(i = 11, s = "lol"))  ==> """{"i":11}"""
        write(FooDefault(i = 10, s = "lol"))  ==> """{}"""
        write(FooDefault())                   ==> """{}"""
      }
    }

    'sources{
      import upickle.default._
      val original = """{"myFieldA":1,"myFieldB":"gg"}"""
      read[Thing](original) ==> Thing(1, "gg")
      read[Thing](original: CharSequence) ==> Thing(1, "gg")
      read[Thing](original.getBytes) ==> Thing(1, "gg")

      import java.nio.file.Files
      val f = Files.createTempFile("", "")
      Files.write(f, original.getBytes)

      read[Thing](f) ==> Thing(1, "gg")
      read[Thing](f.toFile) ==> Thing(1, "gg")
      read[Thing](Files.newByteChannel(f)) ==> Thing(1, "gg")
    }
    'transforms{
      'json{
        import upickle.default._
        transform(1).to[Js.Value] ==> Js.Num(1)
        transform("hello").to[Js.Value] ==> Js.Str("hello")
        transform(("hello", 9)).to[Js.Value] ==> Js.Arr(Js.Str("hello"), Js.Num(9))
        transform(Thing(3, "3")).to[Js.Value] ==>
          Js.Obj("myFieldA" -> Js.Num(3), "myFieldB" -> Js.Str("3"))

        transform(Js.Num(1)).to[Int] ==> 1
        transform(Js.Str("hello")).to[String] ==> "hello"
        transform(Js.Arr(Js.Str("hello"), Js.Num(9))).to[(String, Int)] ==> ("hello", 9)
        transform(Js.Obj("myFieldA" -> Js.Num(3), "myFieldB" -> Js.Str("3"))).to[Thing] ==>
          Thing(3, "3")
      }

      'defaultTransform{

        // upickle.default.transform can be used to convert between
        // JSON-equivalent data-structures without an intermediate AST
        upickle.default.transform(Seq(1, 2, 3)).to[(Int, Int, Int)] ==> (1, 2, 3)

        val bar = Bar("omg", Seq(Foo(1), Foo(2)))

        upickle.default.transform(bar).to[Map[String, Js.Value]] ==>
          Map(
            "name" -> Js.Str("omg"),
            "foos" -> Js.Arr(
              Js.Obj("i" -> Js.Num(1)),
              Js.Obj("i" -> Js.Num(2))
            )
          )

      }
      'misc{
        // ujson.transform is long-hand for upickle.default.{read,write,transform}
        ujson.transform("[1, 2, 3]", upickle.default.reader[Seq[Int]]) ==>
          Seq(1, 2, 3)

        ujson.transform(upickle.default.writable(Seq(1, 2, 3)), StringRenderer()).toString ==>
          "[1,2,3]"

        // It can be used for parsing JSON into an AST
        val exampleAst = Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3))

        ujson.transform("[1, 2, 3]", Js.Builder) ==> exampleAst

        // Rendering the AST to a string
        ujson.transform(exampleAst, StringRenderer()).toString ==> "[1,2,3]"

        // Or to a byte array
        ujson.transform(exampleAst, BytesRenderer()).toBytes ==> "[1,2,3]".getBytes

        // Re-formatting JSON, either compacting it
        ujson.transform("[1, 2, 3]", StringRenderer()).toString ==> "[1,2,3]"

        // or indenting it
        ujson.transform("[1, 2, 3]", StringRenderer(indent = 4)).toString ==>
          """[
            |    1,
            |    2,
            |    3
            |]""".stripMargin

        // `transform` takes any `Transformable`, including byte arrays and files
        ujson.transform("[1, 2, 3]".getBytes, StringRenderer()).toString ==> "[1,2,3]"

        // `transform` can also be used for validating JSON without constructing anything
        ujson.transform("[1, 2, 3]", NoOpVisitor)

        intercept[IncompleteParseException]{
          ujson.transform("[1, 2, 3", NoOpVisitor)
        }
        intercept[ParseException]{
          ujson.transform("[1, 2, 3]]", NoOpVisitor)
        }
      }
    }
    'mapped{
      import upickle.default._
      case class Wrap(i: Int)
      implicit val fooReadWrite: ReadWriter[Wrap] = readwriter[Int].bimap[Wrap](_.i, Wrap(_))
      write(Seq(Wrap(1), Wrap(10), Wrap(100))) ==> "[1,10,100]"
      read[Seq[Wrap]]("[1,10,100]")            ==> Seq(Wrap(1), Wrap(10), Wrap(100))
    }
    'keyed{
      import upickle.default._
      'attrs{
        write(KeyBar(10))                     ==> """{"hehehe":10}"""
        read[KeyBar]("""{"hehehe": 10}""")    ==> KeyBar(10)
      }
      'tag{
        write(B(10))                          ==> """{"$type":"Bee","i":10}"""
        read[B]("""{"$type":"Bee","i":10}""") ==> B(10)
      }
      'snakeCase{
        object SnakePickle extends upickle.AttributeTagged{
          def camelToSnake(s: String) = {
            s.split("(?=[A-Z])", -1).map(_.toLowerCase).mkString("_")
          }
          def snakeToCamel(s: String) = {
            val res = s.split("_", -1).map(x => x(0).toUpper + x.drop(1)).mkString
            s(0).toLower + res.drop(1)
          }


          override def objectAttributeKeyReadMap(s: CharSequence) = snakeToCamel(s.toString)
          override def objectAttributeKeyWriteMap(s: CharSequence) = camelToSnake(s.toString)

          override def objectTypeKeyReadMap(s: CharSequence) = snakeToCamel(s.toString)
          override def objectTypeKeyWriteMap(s: CharSequence) = camelToSnake(s.toString)
        }
        // Default read-writing
        upickle.default.write(Thing(1, "gg")) ==> """{"myFieldA":1,"myFieldB":"gg"}"""
        upickle.default.read[Thing]("""{"myFieldA":1,"myFieldB":"gg"}""") ==> Thing(1, "gg")

        implicit def thingRW: SnakePickle.ReadWriter[Thing] = SnakePickle.macroRW
        // snake_case_keys read-writing
        SnakePickle.write(Thing(1, "gg")) ==> """{"my_field_a":1,"my_field_b":"gg"}"""
        SnakePickle.read[Thing]("""{"my_field_a":1,"my_field_b":"gg"}""") ==> Thing(1, "gg")
      }
    }
  }
}


