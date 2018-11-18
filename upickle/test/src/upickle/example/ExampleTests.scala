package upickle.example

import java.io.StringWriter

import acyclic.file
import upickle.{TestUtil, default}
import utest._
import upickle.default.{macroRW, ReadWriter => RW}
import ujson.{IncompleteParseException, ParseException, Readable}
import ujson.{BytesRenderer, Js, StringRenderer}
import upickle.core.{NoOpVisitor, Visitor}
object Simple {
  case class Thing(myFieldA: Int, myFieldB: String)
  object Thing{
    implicit val rw: RW[Thing] = macroRW
  }
  case class Big(i: Int, b: Boolean, str: String, c: Char, t: Thing)
  object Big{
    implicit val rw: RW[Big] = macroRW
  }
}
object Sealed{
  sealed trait IntOrTuple
  object IntOrTuple{
    implicit val rw: RW[IntOrTuple] = RW.merge(IntThing.rw, TupleThing.rw)
  }
  case class IntThing(i: Int) extends IntOrTuple
  object IntThing{
    implicit val rw: RW[IntThing] = macroRW
  }
  case class TupleThing(name: String, t: (Int, Int)) extends IntOrTuple
  object TupleThing{
    implicit val rw: RW[TupleThing] = macroRW
  }
}
object Recursive{
  case class Foo(i: Int)
  object Foo{
    implicit val rw: RW[Foo] = macroRW
  }
  case class Bar(name: String, foos: Seq[Foo])
  object Bar{
    implicit val rw: RW[Bar] = macroRW
  }
}
object Defaults{
  case class FooDefault(i: Int = 10, s: String = "lol")
  object FooDefault{
    implicit val rw: RW[FooDefault] = macroRW
  }
}
object Keyed{
  case class KeyBar(@upickle.implicits.key("hehehe") kekeke: Int)
  object KeyBar{
    implicit val rw: RW[KeyBar] = macroRW
  }
}
object KeyedTag{
  sealed trait A
  object A{
    implicit val rw: RW[A] = RW.merge(B.rw, macroRW[C.type])
  }
  @upickle.implicits.key("Bee") case class B(i: Int) extends A
  object B{
    implicit val rw: RW[B] = macroRW
  }
  case object C extends A
}
object Custom2{
  class CustomThing2(val i: Int, val s: String)
  object CustomThing2 {
    implicit val rw = upickle.default.readwriter[String].bimap[CustomThing2](
      x => x.i + " " + x.s,
      str => {
        val Array(i, s) = str.split(" ", 2)
        new CustomThing2(i.toInt, s)
      }
    )
  }
}

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

      read[Seq[Int]]("[1,2,3]")         ==> List(1, 2, 3)

      write((1, "omg", true))           ==> """[1,"omg",true]"""

      read[(Int, String, Boolean)]("""[1,"omg",true]""") ==> (1, "omg", true)
    }
    'binary{
      import upickle.default._

      writeBinary(1)                          ==> Array(1)

      writeBinary(Seq(1, 2, 3))               ==> Array(0x93.toByte, 1, 2, 3)

      readBinary[Seq[Int]](Array[Byte](0x93.toByte, 1, 2, 3))  ==> List(1, 2, 3)

      val serializedTuple = Array[Byte](0x93.toByte, 1, 0xa3.toByte, 111, 109, 103, 0xc3.toByte)

      writeBinary((1, "omg", true))           ==> serializedTuple

      readBinary[(Int, String, Boolean)](serializedTuple) ==> (1, "omg", true)
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
        write(12: Long)                   ==> "12"
        write(4000000000000L: Long)       ==> "4000000000000"
        // large longs are written as strings, to avoid floating point rounding
        write(9223372036854775807L: Long) ==> "\"9223372036854775807\""
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
    }
    'mapped{
      'simple {
        import upickle.default._
        case class Wrap(i: Int)
        implicit val fooReadWrite: ReadWriter[Wrap] =
          readwriter[Int].bimap[Wrap](_.i, Wrap(_))

        write(Seq(Wrap(1), Wrap(10), Wrap(100))) ==> "[1,10,100]"
        read[Seq[Wrap]]("[1,10,100]") ==> Seq(Wrap(1), Wrap(10), Wrap(100))
      }
      'Js {
        import upickle.default._
        case class Bar(i: Int, s: String)
        implicit val fooReadWrite: ReadWriter[Bar] =
          readwriter[ujson.Value].bimap[Bar](
            x => ujson.Arr(x.s, x.i),
            json => new Bar(json(1).num.toInt, json(0).str)
          )

        write(Bar(123, "abc")) ==> """["abc",123]"""
        read[Bar]("""["abc",123]""") ==> Bar(123, "abc")
      }
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

          override def objectAttributeKeyReadMap(s: CharSequence) =
            snakeToCamel(s.toString)
          override def objectAttributeKeyWriteMap(s: CharSequence) =
            camelToSnake(s.toString)

          override def objectTypeKeyReadMap(s: CharSequence) =
            snakeToCamel(s.toString)
          override def objectTypeKeyWriteMap(s: CharSequence) =
            camelToSnake(s.toString)
        }

        // Default read-writing
        upickle.default.write(Thing(1, "gg")) ==>
          """{"myFieldA":1,"myFieldB":"gg"}"""

        upickle.default.read[Thing]("""{"myFieldA":1,"myFieldB":"gg"}""") ==>
          Thing(1, "gg")

        implicit def thingRW: SnakePickle.ReadWriter[Thing] = SnakePickle.macroRW

        // snake_case_keys read-writing
        SnakePickle.write(Thing(1, "gg")) ==>
          """{"my_field_a":1,"my_field_b":"gg"}"""

        SnakePickle.read[Thing]("""{"my_field_a":1,"my_field_b":"gg"}""") ==>
          Thing(1, "gg")
      }

      'stringLongs{
        upickle.default.write(123: Long) ==> "123"
        upickle.default.write(Long.MaxValue) ==> "\"9223372036854775807\""

        object StringLongs extends upickle.AttributeTagged{
          override implicit val LongWriter = new Writer[Long] {
            def write0[V](out: Visitor[_, V], v: Long) = out.visitString(v.toString, -1)
          }
        }

        StringLongs.write(123: Long) ==> "\"123\""
        StringLongs.write(Long.MaxValue) ==> "\"9223372036854775807\""

        object NumLongs extends upickle.AttributeTagged{
          override implicit val LongWriter = new Writer[Long] {
            def write0[V](out: Visitor[_, V], v: Long) = out.visitFloat64String(v.toString, -1)
          }
        }

        NumLongs.write(123: Long) ==> "123"
        NumLongs.write(Long.MaxValue) ==> "9223372036854775807"

      }
    }

    'transform{
      upickle.default.transform(Foo(123)).to[Foo] ==> Foo(123)
      val big = Big(1, true, "lol", 'Z', Thing(7, ""))
      upickle.default.transform(big).to[Big] ==> big
    }
    'msgConstruction{
      val msg = upack.Arr(
        upack.Obj(upack.Str("myFieldA") -> upack.Int32(1), upack.Str("myFieldB") -> upack.Str("g")),
        upack.Obj(upack.Str("myFieldA") -> upack.Int32(2), upack.Str("myFieldB") -> upack.Str("k"))
      )

      val binary: Array[Byte] = upack.write(msg)

      val read = upack.read(binary)
      assert(msg == read)
    }

    'msgReadWrite{
      val big = Big(1, true, "lol", 'Z', Thing(7, ""))
      val msg: upack.Msg = upickle.default.writeMsg(big)
      upickle.default.readBinary[Big](msg) ==> big
    }

    'msgInsideValue{
      val msgSeq = Seq[upack.Msg](
        upack.Str("hello world"),
        upack.Arr(upack.Int32(1), upack.Int32(2))
      )

      val binary: Array[Byte] = upickle.default.writeBinary(msgSeq)

      upickle.default.readBinary[Seq[upack.Msg]](binary) ==> msgSeq
    }

    'msgToJson{
      val msg = upack.Arr(
        upack.Obj(upack.Str("myFieldA") -> upack.Int32(1), upack.Str("myFieldB") -> upack.Str("g")),
        upack.Obj(upack.Str("myFieldA") -> upack.Int32(2), upack.Str("myFieldB") -> upack.Str("k"))
      )

      val binary: Array[Byte] = upack.write(msg)

      // Can pretty-print starting from either the upack.Msg structs,
      // or the raw binary data
      upack.transform(msg, new ujson.StringRenderer()).toString ==>
        """[{"myFieldA":1,"myFieldB":"g"},{"myFieldA":2,"myFieldB":"k"}]"""

      upack.transform(binary, new ujson.StringRenderer()).toString ==>
        """[{"myFieldA":1,"myFieldB":"g"},{"myFieldA":2,"myFieldB":"k"}]"""

      // Some messagepack structs cannot be converted to valid JSON, e.g.
      // they may have maps with non-string keys. These can still be pretty-printed:
      val msg2 = upack.Obj(upack.Arr(upack.Int32(1), upack.Int32(2)) -> upack.Int32(1))
      upack.transform(msg2, new ujson.StringRenderer()).toString ==> """{[1,2]:1}"""
    }
    'json{
      'construction{
        import ujson.Js

        val json0 = ujson.Arr(
          ujson.Obj("myFieldA" -> ujson.Num(1), "myFieldB" -> ujson.Str("g")),
          ujson.Obj("myFieldA" -> ujson.Num(2), "myFieldB" -> ujson.Str("k"))
        )

        val json = ujson.Arr( // The `ujson.Num` and `ujson.Str` calls are optional
          ujson.Obj("myFieldA" -> 1, "myFieldB" -> "g"),
          ujson.Obj("myFieldA" -> 2, "myFieldB" -> "k")
        )

        json0 ==> json
        json.toString ==> """[{"myFieldA":1,"myFieldB":"g"},{"myFieldA":2,"myFieldB":"k"}]"""

        val json2 = ujson.Obj(
          "hello" -> (0 until 5),
          "world" -> (0 until 5).map(i => (i.toString, i))
        )

        json2.toString ==> """{"hello":[0,1,2,3,4],"world":{"0":0,"1":1,"2":2,"3":3,"4":4}}"""
      }
      'simple{
        val str = """[{"myFieldA":1,"myFieldB":"g"},{"myFieldA":2,"myFieldB":"k"}]"""
        val json = ujson.read(str)
        json(0)("myFieldA").num   ==> 1
        json(0)("myFieldB").str   ==> "g"
        json(1)("myFieldA").num   ==> 2
        json(1)("myFieldB").str   ==> "k"

        ujson.write(json)         ==> str
      }
      'mutable{
        val str = """[{"myFieldA":1,"myFieldB":"g"},{"myFieldA":2,"myFieldB":"k"}]"""
        val json: ujson.Js = ujson.read(str)

        json.arr.remove(1)
        json(0)("myFieldA") = 1337
        json(0)("myFieldB") = json(0)("myFieldB").str + "lols"

        ujson.write(json) ==> """[{"myFieldA":1337,"myFieldB":"glols"}]"""
      }
      'update{
        val str = """[{"myFieldA":1,"myFieldB":"g"},{"myFieldA":2,"myFieldB":"k"}]"""
        val json: ujson.Js = ujson.read(str)

        json(0)("myFieldA") = _.num + 100
        json(1)("myFieldB") = _.str + "lol"

        val expected = """[{"myFieldA":101,"myFieldB":"g"},{"myFieldA":2,"myFieldB":"klol"}]"""
        ujson.write(json) ==> expected
      }
      'intermediate{
        val data = Seq(Thing(1, "g"), Thing(2, "k"))
        val json = upickle.default.writeJs(data)

        json.arr.remove(1)
        json(0)("myFieldA") = 1337

        upickle.default.read[Seq[Thing]](json)   ==> Seq(Thing(1337, "g"))
      }
      'copy{
        val data = ujson.Obj(
          "hello" -> 1,
          "world" -> 2
        )

        val data2 = ujson.copy(data)

        data("hello") = 3
        data2("hello").num ==> 1
      }
    }
    'transforms{
      'json{
        import upickle.default._
        transform(1).to[ujson.Value] ==> ujson.Num(1)
        transform("hello").to[ujson.Value] ==> ujson.Str("hello")
        transform(("hello", 9)).to[ujson.Value] ==> ujson.Arr("hello", 9)
        transform(Thing(3, "3")).to[ujson.Value] ==>
          ujson.Obj("myFieldA" -> 3, "myFieldB" -> "3")

        transform(ujson.Num(1)).to[Int] ==> 1
        transform(ujson.Str("hello")).to[String] ==> "hello"
        transform(ujson.Arr("hello", 9)).to[(String, Int)] ==> ("hello", 9)
        transform(ujson.Obj("myFieldA" -> 3, "myFieldB" -> "3")).to[Thing] ==>
          Thing(3, "3")
      }

      'defaultTransform{

        // upickle.default.transform can be used to convert between
        // JSON-equivalent data-structures without an intermediate AST
        upickle.default.transform(Seq(1, 2, 3)).to[(Int, Int, Int)] ==> (1, 2, 3)

        val bar = Bar("omg", Seq(Foo(1), Foo(2)))

        upickle.default.transform(bar).to[Map[String, ujson.Value]] ==>
          Map[String, ujson.Value](
            "name" -> "omg",
            "foos" -> ujson.Arr(
              ujson.Obj("i" -> 1),
              ujson.Obj("i" -> 2)
            )
          )

      }
      'misc {
        // It can be used for parsing JSON into an AST
        val exampleAst = ujson.Arr(1, 2, 3)

        ujson.transform("[1, 2, 3]", Js) ==> exampleAst

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

      }
      'validate {
        ujson.transform("[1, 2, 3]", NoOpVisitor)

        intercept[IncompleteParseException](
          ujson.transform("[1, 2, 3", NoOpVisitor)
        )
        intercept[ParseException](
          ujson.transform("[1, 2, 3]]", NoOpVisitor)
        )
      }
      'upickleDefault{
        ujson.transform("[1, 2, 3]", upickle.default.reader[Seq[Int]]) ==>
          Seq(1, 2, 3)

        ujson.transform(upickle.default.transform(Seq(1, 2, 3)), StringRenderer()).toString ==>
          "[1,2,3]"
      }
    }
  }
}


