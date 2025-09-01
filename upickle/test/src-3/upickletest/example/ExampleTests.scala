package upickletest.example

import java.io.StringWriter

import upickletest.{TestUtil}
import utest._
import upickle.{macroRW, ReadWriter => RW}
import ujson.{IncompleteParseException, ParseException}
import ujson.{BytesRenderer, Value, StringRenderer, Readable}
import upickle.core.{NoOpVisitor, Visitor}

object Simple {
  case class Thing(myFieldA: Int, myFieldB: String)
  object Thing{
    implicit val rw: RW[Thing] = macroRW
  }
  case class OptionThing(myFieldA: Option[Int] = None, myFieldB: Option[String] = None)
  object OptionThing{
    implicit val rw: RW[OptionThing] = macroRW
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
object KeyedTagKey {
  @upickle.implicits.key("_tag")
  sealed trait Tag
  case class ATag(i: Int) extends Tag
  object ATag {
    implicit val rw: RW[ATag] = macroRW
  }
}
object Custom2{
  class CustomThing2(val i: Int, val s: String)
  object CustomThing2 {
    implicit val rw: RW[CustomThing2] = upickle.readwriter[String].bimap[CustomThing2](
      x => s"${x.i} ${x.s}",
      str => {
        val Array(i, s) = str.split(" ", 2)
        new CustomThing2(i.toInt, s)
      }
    )
  }
}



import KeyedTag._
import KeyedTagKey._
import Keyed._
import Sealed._
import Simple._
import Recursive._
import Defaults._

object ExampleTests extends TestSuite {
  object Generic {
    case class Container[T](a: T)
    object Container{
      implicit val intRW: RW[Container[Int]] = macroRW
    }
  }

  import TestUtil._
  val tests = Tests {
    test("simple"){
      upickle.write(1)                          ==> "1"

      upickle.write(Seq(1, 2, 3))               ==> "[1,2,3]"

      upickle.read[Seq[Int]]("[1,2,3]")         ==> List(1, 2, 3)

      upickle.write((1, "omg", true))           ==> """[1,"omg",true]"""

      upickle.read[(Int, String, Boolean)]("""[1,"omg",true]""") ==> (1, "omg", true)
    }
    test("binary"){
      upickle.writeBinary(1)                          ==> Array(1)

      upickle.writeBinary(Seq(1, 2, 3))               ==> Array(0x93.toByte, 1, 2, 3)

      upickle.readBinary[Seq[Int]](Array[Byte](0x93.toByte, 1, 2, 3))  ==> List(1, 2, 3)

      val serializedTuple = Array[Byte](0x93.toByte, 1, 0xa3.toByte, 111, 109, 103, 0xc3.toByte)

      upickle.writeBinary((1, "omg", true))           ==> serializedTuple

      upickle.readBinary[(Int, String, Boolean)](serializedTuple) ==> (1, "omg", true)
    }
    test("more"){
      test("booleans"){
        upickle.write(true: Boolean)              ==> "true"
        upickle.write(false: Boolean)             ==> "false"
      }
      test("numbers"){
        upickle.write(12: Int)                    ==> "12"
        upickle.write(12: Short)                  ==> "12"
        upickle.write(12: Byte)                   ==> "12"
        upickle.write(Int.MaxValue)               ==> "2147483647"
        upickle.write(Int.MinValue)               ==> "-2147483648"
        upickle.write(12.5f: Float)               ==> "12.5"
        upickle.write(12.5: Double)               ==> "12.5"
      }
      test("longs"){
        upickle.write(12: Long)                   ==> "12"
        upickle.write(4000000000000L: Long)       ==> "4000000000000"
        // large longs are written as strings, to avoid floating point rounding
        upickle.write(9223372036854775807L: Long) ==> "\"9223372036854775807\""
      }
      test("specialNumbers"){
        upickle.write(1.0/0: Double)              ==> "\"Infinity\""
        upickle.write(Float.PositiveInfinity)     ==> "\"Infinity\""
        upickle.write(Float.NegativeInfinity)     ==> "\"-Infinity\""
      }
      test("charStrings"){
        upickle.write('o')                        ==> "\"o\""
        upickle.write("omg")                      ==> "\"omg\""
      }
      test("seqs"){
        upickle.write(Array.empty[Int])           ==> "[]"
        upickle.write(Array(1, 2, 3))             ==> "[1,2,3]"

        // You can pass in an `indent` parameter to format it nicely
        upickle.write(Array.empty[Int], indent = 4)  ==> "[]"
        upickle.write(Array(1, 2, 3), indent = 4)  ==>
          """[
            |    1,
            |    2,
            |    3
            |]""".stripMargin

        upickle.write(Seq(1, 2, 3))               ==> "[1,2,3]"
        upickle.write(Vector(1, 2, 3))            ==> "[1,2,3]"
        upickle.write(List(1, 2, 3))              ==> "[1,2,3]"
        import collection.immutable.SortedSet
        upickle.write(SortedSet(1, 2, 3))         ==> "[1,2,3]"
      }
      test("maps"){
        upickle.write(Map(1 -> 2, 3 -> 4))         ==> """{"1":2,"3":4}"""
        upickle.write(Map("hello" -> "world"))     ==> """{"hello":"world"}"""
        upickle.write(Map(Seq(1, 2) -> Seq(3, 4))) ==> """[[[1,2],[3,4]]]"""
        upickle.write(Map.empty[Int, Int])         ==> """{}"""
        upickle.write(Map(Seq.empty[Int] -> Seq.empty[Int])) ==> """[[[],[]]]"""

        upickle.write(Map(Seq.empty[Int] -> Seq.empty[Int]), indent = 4) ==>
        """[
          |    [
          |        [],
          |        []
          |    ]
          |]""".stripMargin

        upickle.write(Map.empty[Int, Int], indent = 4) ==> """{}"""
      }
      test("options"){
        upickle.write(Some(1))                    ==> "1"
        upickle.write(None)                       ==> "null"
      }

      test("either") {
        upickle.write(Left(42): Either[Int, String])     ==> "[0,42]"
        upickle.write(Right("foo"): Either[Int, String]) ==> """[1,"foo"]"""
      }

      test("tuples"){
        upickle.write((1, "omg"))                 ==> """[1,"omg"]"""
        upickle.write((1, "omg", true))           ==> """[1,"omg",true]"""
      }

      test("caseClass"){
        import upickle._
        upickle.write(Thing(1, "gg"))             ==> """{"myFieldA":1,"myFieldB":"gg"}"""
        upickle.read[Thing]("""{"myFieldA":1,"myFieldB":"gg"}""") ==> Thing(1, "gg")
        upickle.write(Big(1, true, "lol", 'Z', Thing(7, ""))) ==>
          """{"i":1,"b":true,"str":"lol","c":"Z","t":{"myFieldA":7,"myFieldB":""}}"""

        upickle.write(Big(1, true, "lol", 'Z', Thing(7, "")), indent = 4) ==>
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

        upickle.write(Big(1, true, "lol", 'Z', Thing(7, "")), indent = 4, sortKeys = true) ==>
          """{
            |    "b": true,
            |    "c": "Z",
            |    "i": 1,
            |    "str": "lol",
            |    "t": {
            |        "myFieldA": 7,
            |        "myFieldB": ""
            |    }
            |}""".stripMargin
      }

      test("caseClassOption"){
        upickle.write(OptionThing(Some(1), Some("gg")))             ==> """{"myFieldA":1,"myFieldB":"gg"}"""
        upickle.read[OptionThing]("""{"myFieldA":1,"myFieldB":"gg"}""") ==> OptionThing(Some(1), Some("gg"))

        upickle.write(OptionThing(Some(1), None))             ==> """{"myFieldA":1}"""
        upickle.read[OptionThing]("""{"myFieldA":1}""") ==> OptionThing(Some(1), None)
      }

      test("sealed"){
        upickle.write(IntThing(1)) ==> """{"$type":"IntThing","i":1}"""

        upickle.write(TupleThing("naeem", (1, 2))) ==>
          """{"$type":"TupleThing","name":"naeem","t":[1,2]}"""

        // You can read tagged value without knowing its
        // type in advance, just use type of the sealed trait
        upickle.read[IntOrTuple]("""{"$type":"IntThing","i":1}""") ==> IntThing(1)

      }
      test("recursive"){
        upickle.write((((1, 2), (3, 4)), ((5, 6), (7, 8)))) ==>
          """[[[1,2],[3,4]],[[5,6],[7,8]]]"""

        upickle.write(Seq(Thing(1, "g"), Thing(2, "k"))) ==>
          """[{"myFieldA":1,"myFieldB":"g"},{"myFieldA":2,"myFieldB":"k"}]"""

        upickle.write(Bar("bearrr", Seq(Foo(1), Foo(2), Foo(3)))) ==>
          """{"name":"bearrr","foos":[{"i":1},{"i":2},{"i":3}]}"""
      }
      test("null"){
        upickle.write(Bar(null, Seq(Foo(1), null, Foo(3)))) ==>
          """{"name":null,"foos":[{"i":1},null,{"i":3}]}"""
      }
    }
    test("defaults"){
      test("reading"){
        upickle.read[FooDefault]("{}")                ==> FooDefault(10, "lol")
        upickle.read[FooDefault]("""{"i": 123}""")    ==> FooDefault(123,"lol")
      }
      test("writing"){
        upickle.write(FooDefault(i = 11, s = "lol"))  ==> """{"i":11}"""
        upickle.write(FooDefault(i = 10, s = "lol"))  ==> """{}"""
        upickle.write(FooDefault())                   ==> """{}"""
      }
    }

    test("sources"){
      val original = """{"myFieldA":1,"myFieldB":"gg"}"""
      upickle.read[Thing](original) ==> Thing(1, "gg")
      upickle.read[Thing](original: CharSequence) ==> Thing(1, "gg")
      upickle.read[Thing](original.getBytes) ==> Thing(1, "gg")
    }
    test("mapped"){
      test("simple"){
        case class Wrap(i: Int)
        implicit val fooReadWrite: upickle.ReadWriter[Wrap] =
          upickle.readwriter[Int].bimap[Wrap](_.i, Wrap(_))

        upickle.write(Seq(Wrap(1), Wrap(10), Wrap(100))) ==> "[1,10,100]"
        upickle.read[Seq[Wrap]]("[1,10,100]") ==> Seq(Wrap(1), Wrap(10), Wrap(100))
      }
      test("Value"){
        case class Bar(i: Int, s: String)
        implicit val fooReadWrite: upickle.ReadWriter[Bar] =
          upickle.readwriter[ujson.Value].bimap[Bar](
            x => ujson.Arr(x.s, x.i),
            json => new Bar(json(1).num.toInt, json(0).str)
          )

        upickle.write(Bar(123, "abc")) ==> """["abc",123]"""
        upickle.read[Bar]("""["abc",123]""") ==> Bar(123, "abc")
      }
    }
    test("keyed"){
      test("attrs"){
        upickle.write(KeyBar(10))                     ==> """{"hehehe":10}"""
        upickle.read[KeyBar]("""{"hehehe": 10}""")    ==> KeyBar(10)
      }
      test("tag"){
        upickle.write(B(10))                          ==> """{"$type":"Bee","i":10}"""
        upickle.read[B]("""{"$type":"Bee","i":10}""") ==> B(10)
      }
      test("tagKey"){
        upickle.write(ATag(11)) ==>
          """{"_tag":"ATag","i":11}"""

        upickle.read[ATag]("""{"_tag":"ATag","i":11}""") ==>
          ATag(11)
      }
      test("snakeCase"){
        object SnakePickle extends upickle.AttributeTagged{
          def camelToSnake(s: String) = {
            s.replaceAll("([A-Z])","#$1").split('#').map(_.toLowerCase).mkString("_")
          }
          def snakeToCamel(s: String) = {
            val res = s.split("_", -1).map(x => s"${x(0).toUpper}${x.drop(1)}").mkString
            s"${s(0).toLower}${res.drop(1)}"
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
        upickle.write(Thing(1, "gg")) ==>
          """{"myFieldA":1,"myFieldB":"gg"}"""

        upickle.read[Thing]("""{"myFieldA":1,"myFieldB":"gg"}""") ==>
          Thing(1, "gg")

        implicit def thingRW: SnakePickle.ReadWriter[Thing] = SnakePickle.macroRW

        // snake_case_keys read-writing
        SnakePickle.write(Thing(1, "gg")) ==>
          """{"my_field_a":1,"my_field_b":"gg"}"""

        SnakePickle.read[Thing]("""{"my_field_a":1,"my_field_b":"gg"}""") ==>
          Thing(1, "gg")
      }

      test("stringLongs"){
        upickle.write(123: Long) ==> "123"
        upickle.write(Long.MaxValue) ==> "\"9223372036854775807\""

        object StringLongs extends upickle.AttributeTagged{
          override implicit val LongWriter: Writer[Long] = new Writer[Long] {
            def write0[V](out: Visitor[_, V], v: Long) = out.visitString(v.toString, -1)
          }
        }

        StringLongs.write(123: Long) ==> "\"123\""
        StringLongs.write(Long.MaxValue) ==> "\"9223372036854775807\""

        object NumLongs extends upickle.AttributeTagged{
          override implicit val LongWriter: Writer[Long] = new Writer[Long] {
            def write0[V](out: Visitor[_, V], v: Long) = out.visitFloat64String(v.toString, -1)
          }
        }

        NumLongs.write(123: Long) ==> "123"
        NumLongs.write(Long.MaxValue) ==> "9223372036854775807"

      }

      test("serializeDefaults"){
        object SerializeDefaults extends upickle.AttributeTagged{
          override def serializeDefaults = true
        }
        implicit val fooDefaultRW: SerializeDefaults.ReadWriter[FooDefault] = SerializeDefaults.macroRW
        SerializeDefaults.write(FooDefault(i = 11, s = "lol"))  ==> """{"i":11,"s":"lol"}"""
        SerializeDefaults.write(FooDefault(i = 10, s = "lol"))  ==> """{"i":10,"s":"lol"}"""
        SerializeDefaults.write(FooDefault())                   ==> """{"i":10,"s":"lol"}"""
      }
    }

    test("transform"){
      upickle.transform(Foo(123)).to[Foo] ==> Foo(123)
      val big = Big(1, true, "lol", 'Z', Thing(7, ""))
      upickle.transform(big).to[Big] ==> big
    }
    test("msgConstruction"){
      val msg = upack.Arr(
        upack.Obj(upack.Str("myFieldA") -> upack.Int32(1), upack.Str("myFieldB") -> upack.Str("g")),
        upack.Obj(upack.Str("myFieldA") -> upack.Int32(2), upack.Str("myFieldB") -> upack.Str("k"))
      )

      val binary: Array[Byte] = upack.write(msg)

      val read = upack.read(binary)
      assert(msg == read)
    }

    test("msgReadWrite"){
      val big = Big(1, true, "lol", 'Z', Thing(7, ""))
      val msg: upack.Msg = upickle.writeMsg(big)
      upickle.readBinary[Big](msg) ==> big
    }

    test("msgInsideValue"){
      val msgSeq = Seq[upack.Msg](
        upack.Str("hello world"),
        upack.Arr(upack.Int32(1), upack.Int32(2))
      )

      val binary: Array[Byte] = upickle.writeBinary(msgSeq)

      upickle.readBinary[Seq[upack.Msg]](binary) ==> msgSeq
    }

    test("msgToJson"){
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
      upack.transform(msg2, new ujson.StringRenderer()).toString ==> """{"[1,2]":1}"""
    }
    test("json"){
      test("construction"){

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
      test("simple"){
        val str = """[{"myFieldA":1,"myFieldB":"g"},{"myFieldA":2,"myFieldB":"k"}]"""
        val json = ujson.read(str)
        json(0)("myFieldA").num   ==> 1
        json(0)("myFieldB").str   ==> "g"
        json(1)("myFieldA").num   ==> 2
        json(1)("myFieldB").str   ==> "k"

        ujson.write(json)         ==> str
      }
      test("mutable"){
        val str = """[{"myFieldA":1,"myFieldB":"g"},{"myFieldA":2,"myFieldB":"k"}]"""
        val json: ujson.Value = ujson.read(str)

        json.arr.remove(1)
        json(0)("myFieldA") = 1337
        json(0)("myFieldB") = json(0)("myFieldB").str + "lols"

        ujson.write(json) ==> """[{"myFieldA":1337,"myFieldB":"glols"}]"""
      }
      test("update"){
        val str = """[{"myFieldA":1,"myFieldB":"g"},{"myFieldA":2,"myFieldB":"k"}]"""
        val json: ujson.Value = ujson.read(str)

        json(0)("myFieldA") = _.num + 100
        json(1)("myFieldB") = _.str + "lol"

        val expected = """[{"myFieldA":101,"myFieldB":"g"},{"myFieldA":2,"myFieldB":"klol"}]"""
        ujson.write(json) ==> expected
      }
      test("intermediate"){
        val data = Seq(Thing(1, "g"), Thing(2, "k"))
        val json = upickle.writeJs(data)

        json.arr.remove(1)
        json(0)("myFieldA") = 1337

        upickle.read[Seq[Thing]](json)   ==> Seq(Thing(1337, "g"))
      }
      test("copy"){
        val data = ujson.Obj(
          "hello" -> 1,
          "world" -> 2
        )

        val data2 = ujson.copy(data)

        data("hello") = 3
        data2("hello").num ==> 1
      }
    }
    test("transforms"){
      test("json"){
        upickle.transform(1).to[ujson.Value] ==> ujson.Num(1)
        upickle.transform("hello").to[ujson.Value] ==> ujson.Str("hello")
        upickle.transform(("hello", 9)).to[ujson.Value] ==> ujson.Arr("hello", 9)
        upickle.transform(Thing(3, "3")).to[ujson.Value] ==>
          ujson.Obj("myFieldA" -> 3, "myFieldB" -> "3")

        upickle.transform(ujson.Num(1)).to[Int] ==> 1
        upickle.transform(ujson.Str("hello")).to[String] ==> "hello"
        upickle.transform(ujson.Arr("hello", 9)).to[(String, Int)] ==> ("hello", 9)
        upickle.transform(ujson.Obj("myFieldA" -> 3, "myFieldB" -> "3")).to[Thing] ==>
          Thing(3, "3")
      }

      test("defaultTransform"){

        // upickle.transform can be used to convert between
        // JSON-equivalent data-structures without an intermediate AST
        upickle.transform(Seq(1, 2, 3)).to[(Int, Int, Int)] ==> (1, 2, 3)

        val bar = Bar("omg", Seq(Foo(1), Foo(2)))

        upickle.transform(bar).to[Map[String, ujson.Value]] ==>
          Map[String, ujson.Value](
            "name" -> "omg",
            "foos" -> ujson.Arr(
              ujson.Obj("i" -> 1),
              ujson.Obj("i" -> 2)
            )
          )

      }
      test("misc"){
        // It can be used for parsing JSON into an AST
        val exampleAst = ujson.Arr(1, 2, 3)

        ujson.transform("[1, 2, 3]", Value) ==> exampleAst

        // Rendering the AST to a string
        ujson.transform(exampleAst, StringRenderer()).toString ==> "[1,2,3]"

        // Or to a byte array
        ujson.transform(exampleAst, BytesRenderer()).toByteArray ==> "[1,2,3]".getBytes

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
      test("validate"){
        test {
          ujson.transform("[1, 2, 3]", NoOpVisitor)
        }

        test{
          intercept[IncompleteParseException](ujson.transform("[", NoOpVisitor))
        }
        test{
          intercept[IncompleteParseException](ujson.transform("[1, 2, 3", NoOpVisitor))
        }
        test{
          intercept[ParseException](ujson.transform("[1, 2, 3]]", NoOpVisitor))
        }
      }
      test("upickleDefault"){
        ujson.transform("[1, 2, 3]", upickle.reader[Seq[Int]]) ==>
          Seq(1, 2, 3)

        ujson.transform(upickle.transform(Seq(1, 2, 3)), StringRenderer()).toString ==>
          "[1,2,3]"
      }
    }
    test("byteArrays"){
      upickle.write(Array[Byte](1, 2, 3, 4)) ==> "[1,2,3,4]"
      upickle.read[Array[Byte]]("[1,2,3,4]") ==> Array(1, 2, 3, 4)

      upickle.writeBinary(Array[Byte](1, 2, 3, 4)) ==> Array(0xc4.toByte, 4, 1, 2, 3, 4)
      upickle.readBinary[Array[Byte]](Array[Byte](0xc4.toByte, 4, 1, 2, 3, 4)) ==> Array(1, 2, 3, 4)
    }
    test("nonCustomMapKeys") {
      case class FooId(x: Int)
      implicit val fooRW: upickle.ReadWriter[FooId] =
        upickle.readwriter[Int].bimap[FooId](_.x, FooId(_))

      upickle.write(FooId(123)) ==> "123"
      upickle.read[FooId]("123") ==> FooId(123)

      upickle.write(Map(FooId(123) -> "hello", FooId(456) -> "world")) ==>
        """[[123,"hello"],[456,"world"]]"""

      upickle.read[Map[FooId, String]]("""[[123,"hello"],[456,"world"]]""") ==>
        Map(FooId(123) -> "hello", FooId(456) -> "world")

    }
    test("customMapKeys") {
      case class FooId(x: Int)
      implicit val fooRW: upickle.ReadWriter[FooId] =
        upickle.stringKeyRW(upickle.readwriter[Int].bimap[FooId](_.x, FooId(_)))

      upickle.write(FooId(123)) ==> "123"
      upickle.read[FooId]("123") ==> FooId(123)

      upickle.write(Map(FooId(123) -> "hello", FooId(456) -> "world")) ==>
        """{"123":"hello","456":"world"}"""

      upickle.read[Map[FooId, String]]("""{"123":"hello","456":"world"}""") ==>
        Map(FooId(123) -> "hello", FooId(456) -> "world")
    }
    test("msgPackMapKeys") {
      upickle.write(Map(Seq(1) -> 1, Seq(1, 2) -> 3, Seq(1, 2, 3) -> 6)) ==> "[[[1],1],[[1,2],3],[[1,2,3],6]]"

      upickle.read[Map[Seq[Int], Int]]("[[[1],1],[[1,2],3],[[1,2,3],6]]") ==>
        Map(Seq(1) -> 1, Seq(1, 2) -> 3, Seq(1, 2, 3) -> 6)

      upickle.writeMsg(Map(Seq(1) -> 1, Seq(1, 2) -> 3, Seq(1, 2, 3) -> 6)) ==>
        upack.Obj(
          upack.Arr(upack.Int32(1)) -> upack.Int32(1),
          upack.Arr(upack.Int32(1), upack.Int32(2)) -> upack.Int32(3),
          upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3)) -> upack.Int32(6)
        )

      upickle.readBinary[Map[Seq[Int], Int]](
        upack.Obj(
          upack.Arr(upack.Int32(1)) -> upack.Int32(1),
          upack.Arr(upack.Int32(1), upack.Int32(2)) -> upack.Int32(3),
          upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3)) -> upack.Int32(6)
        )
      ) ==>
        Map(Seq(1) -> 1, Seq(1, 2) -> 3, Seq(1, 2, 3) -> 6)
    }
    test("generic"){
      import Generic._
      test("read") - {
        val containerJson = """{"a":3}"""
        val parsed = upickle.read[Container[Int]](containerJson)
        val expected = Container[Int](3)
        assert(parsed == expected)
      }

      test("write") - {
        val original = Container[Int](3)
        val serialized = upickle.write(original)
        val expected = """{"a":3}"""
        assert(serialized == expected)
      }
    }
  }
}


