package upickle

import utest._
import upickle.TestUtil.rw

object shared {
  object that {
    import common.Message
    case class That(common: Message)
    object That{
      implicit def rw: upickle.default.ReadWriter[That] = upickle.default.macroRW
    }
  }
  object other {
    import common.Message
    case class Other(common: Message)
    object Other{
      implicit def rw: upickle.default.ReadWriter[Other] = upickle.default.macroRW
    }
  }
  object common {
    case class Message(content: String)
    object Message{
      implicit def rw: upickle.default.ReadWriter[Message] = upickle.default.macroRW
    }
  }
}

object All {
  import shared.other._
  sealed trait Outers
  object Outers{
    implicit def rw: upickle.default.ReadWriter[Outers] = upickle.default.ReadWriter.merge(
      Out1.rw
    )
  }
  case class Out1(a: Other) extends Outers
  object Out1{
    implicit def rw: upickle.default.ReadWriter[Out1] = upickle.default.macroRW
  }

  import shared.that._
  import shared.common._
  sealed trait Inners extends Outers
  object Inners{
    implicit def rw: upickle.default.ReadWriter[Inners] = upickle.default.ReadWriter.merge(
      Inner1.rw,
      Inner2.rw
    )
  }
  case class Inner1(b: That) extends Inners
  object Inner1{
    implicit def rw: upickle.default.ReadWriter[Inner1] = upickle.default.macroRW
  }
  case class Inner2(a: Message) extends Inners
  object Inner2{
    implicit def rw: upickle.default.ReadWriter[Inner2] = upickle.default.macroRW
  }
}

import upickle.default.{ReadWriter, macroRW}
sealed trait Gadt[T]
object Gadt{
  implicit def rw[T]: ReadWriter[Gadt[T]] = macroRW[Gadt[_]].asInstanceOf[ReadWriter[Gadt[T]]]
  case class IsDir(path: String) extends Gadt[Boolean]
  object IsDir{
    implicit val rw: ReadWriter[IsDir] = macroRW
  }
  case class Exists(path: String) extends Gadt[Boolean]
  object Exists{
    implicit val rw: ReadWriter[Exists] = macroRW
  }
  case class ReadBytes(path: String) extends Gadt[Array[Byte]]
  object ReadBytes{
    implicit val rw: ReadWriter[ReadBytes] = macroRW
  }
  case class CopyOver(src: Seq[Byte], path: String) extends Gadt[Unit]
  object CopyOver{
    implicit val rw: ReadWriter[CopyOver] = macroRW
  }
}

sealed trait Gadt2[T, V]
object Gadt2{
  implicit def rw[T, V: ReadWriter]: ReadWriter[Gadt2[T, V]] =
    macroRW[Gadt2[_, V]].asInstanceOf[ReadWriter[Gadt2[T, V]]]

  case class IsDir[V](v: V) extends Gadt2[Boolean, V]
  object IsDir{
    implicit def rw[V: ReadWriter]: ReadWriter[IsDir[V]] = macroRW
  }
  case class Exists[V](v: V) extends Gadt2[Boolean, V]
  object Exists{
    implicit def rw[V: ReadWriter]: ReadWriter[Exists[V]] = macroRW
  }
  case class ReadBytes[V](v: V) extends Gadt2[Array[Byte], V]
  object ReadBytes{
    implicit def rw[V: ReadWriter]: ReadWriter[ReadBytes[V]] = macroRW
  }
  case class CopyOver[V](src: Seq[Byte], v: String) extends Gadt2[Int, V]
  object CopyOver{
    implicit def rw[V]: ReadWriter[CopyOver[V]] = macroRW
  }
}

object AdvancedTests extends TestSuite {
  import All._
  val tests = Tests {
    "complexTraits" - {
      val reader = implicitly[upickle.default.Reader[Outers]]
      val writer = implicitly[upickle.default.Writer[Outers]]
      assert(reader != null)
      assert(writer != null)
    }
    test("GenericDataTypes"){
      test("simple"){
        import Generic.A
        test - rw(A(1), """{"t":1}""")
        test - rw(A("1"), """{"t":"1"}""")
//        test - rw(A(Seq("1", "2", "3")), """{"t":["1","2","3"]}""")
        test - rw(A(A(A(A(A(A(A(1))))))), """{"t":{"t":{"t":{"t":{"t":{"t":{"t":1}}}}}}}""")
      }
      test("large"){
        import Generic.ADT
        rw(ADT(1, 2, 3, 4, 5, 6), """{"a":1,"b":2,"c":3,"d":4,"e":5,"f":6}""")
        rw(
          ADT(
            ADT(1, 2, 3, 4, 5, 6),
            ADT(1, 2, 3, 4, 5, 6),
            ADT(1, 2, 3, 4, 5, 6),
            ADT(1, 2, 3, 4, 5, 6),
            ADT(1, 2, 3, 4, 5, 6),
            ADT(1, 2, 3, 4, 5, 6)
          ),
          """{"a":{"a":1,"b":2,"c":3,"d":4,"e":5,"f":6},"b":{"a":1,"b":2,"c":3,"d":4,"e":5,"f":6},"c":{"a":1,"b":2,"c":3,"d":4,"e":5,"f":6},"d":{"a":1,"b":2,"c":3,"d":4,"e":5,"f":6},"e":{"a":1,"b":2,"c":3,"d":4,"e":5,"f":6},"f":{"a":1,"b":2,"c":3,"d":4,"e":5,"f":6}}"""
        )
      }
      test("ADT"){
        import GenericADTs._
        test - {
          val pref1 = "upickle.GenericADTs.Delta"
          val D1 = Delta
          type D1[+A, +B] = Delta[A, B]
          rw(D1.Insert(1, 1), s"""{"$$type":"$pref1.Insert","key":1,"value":1}""")
          rw(D1.Insert(1, 1): D1[Int, Int], s"""{"$$type":"$pref1.Insert","key":1,"value":1}""")
          rw(D1.Remove(1), s"""{"$$type":"$pref1.Remove","key":1}""")
          rw(D1.Remove(1): D1[Int, Int], s"""{"$$type":"$pref1.Remove","key":1}""")
          rw(D1.Clear(), s"""{"$$type":"$pref1.Clear"}""")
          rw(D1.Clear(): D1[Int, Int], s"""{"$$type":"$pref1.Clear"}""")
        }
        test - {
          val pref2 = "upickle.GenericADTs.DeltaInvariant"
          val D2 = DeltaInvariant
          type D2[A, B] = DeltaInvariant[A, B]
          rw(D2.Insert(1, 1), s"""{"$$type":"$pref2.Insert","key":1,"value":1}""")
          rw(D2.Insert(1, 1): D2[Int, Int], s"""{"$$type":"$pref2.Insert","key":1,"value":1}""")
          rw(D2.Remove(1), s"""{"$$type":"$pref2.Remove","key":1}""")
          rw(D2.Remove(1): D2[Int, Int], s"""{"$$type":"$pref2.Remove","key":1}""")
          rw(D2.Clear(), s"""{"$$type":"$pref2.Clear"}""")
          rw(D2.Clear(): D2[Int, Int], s"""{"$$type":"$pref2.Clear"}""")
        }
      }
    }

    test("recursiveDataTypes"){
      import Recursive._
      rw(
        IntTree(123, List(IntTree(456, Nil), IntTree(789, Nil))),
        """{
          "value": 123,
          "children": [
            {"value":456,"children":[]},
            {"value":789,"children":[]}
          ]
        }"""
      )
      rw(
        SingleNode(123, List(SingleNode(456, Nil), SingleNode(789, Nil))),
        """{
          "$type": "upickle.Recursive.SingleNode",
          "value": 123,
          "children": [
            {
              "$type": "upickle.Recursive.SingleNode",
              "value": 456,
              "children": []
            },
            {
              "$type": "upickle.Recursive.SingleNode",
              "value":789,
              "children":[]
            }
          ]
        }"""
      )
      rw(
        SingleNode(123, List(SingleNode(456, Nil), SingleNode(789, Nil))): SingleTree,
        """{
          "$type": "upickle.Recursive.SingleNode",
          "value": 123,
          "children": [
            {
              "$type": "upickle.Recursive.SingleNode",
              "value": 456,
              "children": []
            },
            {
              "$type": "upickle.Recursive.SingleNode",
              "value":789,
              "children":[]
            }
          ]
        }"""
      )
      rw(End: LL, """ "upickle.Recursive.End" """, """{"$type":"upickle.Recursive.End"}""")
      rw(Node(3, End): LL,
        """{
          "$type": "upickle.Recursive.Node",
          "c": 3,
          "next": "upickle.Recursive.End"
        }""",
        """{
          "$type": "upickle.Recursive.Node",
          "c": 3,
          "next": {"$type":"upickle.Recursive.End"}
        }"""
      )

      rw(
        Node(6, Node(3, End)),
        """{
          "$type": "upickle.Recursive.Node",
          "c": 6,
          "next": {
            "$type": "upickle.Recursive.Node",
            "c":3,
            "next": "upickle.Recursive.End"
          }
        }""",
        """{
          "$type": "upickle.Recursive.Node",
          "c": 6,
          "next": {
            "$type": "upickle.Recursive.Node",
            "c":3,
            "next":{"$type":"upickle.Recursive.End"}
          }
        }"""
      )

    }
    test("gadt"){
      test("simple"){
        test - rw(Gadt.Exists("hello"), """{"$type":"upickle.Gadt.Exists","path":"hello"}""")
//        test - rw(Gadt.Exists("hello"): Gadt[_], """{"$type":"upickle.Gadt.Exists","path":"hello"}""")
        test - rw(Gadt.IsDir(" "), """{"$type":"upickle.Gadt.IsDir","path":" "}""")
//        test - rw(Gadt.IsDir(" "): Gadt[_], """{"$type":"upickle.Gadt.IsDir","path":" "}""")
        test - rw(Gadt.ReadBytes("\""), """{"$type":"upickle.Gadt.ReadBytes","path":"\""}""")
//        test - rw(Gadt.ReadBytes("\""): Gadt[_], """{"$type":"upickle.Gadt.ReadBytes","path":"\""}""")
        test - rw(Gadt.CopyOver(Seq(1, 2, 3), ""), """{"$type":"upickle.Gadt.CopyOver","src":[1,2,3],"path":""}""")
//        test - rw(Gadt.CopyOver(Seq(1, 2, 3), ""): Gadt[_], """{"$type":"upickle.Gadt.CopyOver","src":[1,2,3],"path":""}""")
      }
      test("partial"){
        test - rw(Gadt2.Exists("hello"), """{"$type":"upickle.Gadt2.Exists","v":"hello"}""")
//        test - rw(Gadt2.Exists("hello"): Gadt2[_, String], """{"$type":"upickle.Gadt2.Exists","v":"hello"}""")
        test - rw(Gadt2.IsDir(123), """{"$type":"upickle.Gadt2.IsDir","v":123}""")
//        test - rw(Gadt2.IsDir(123): Gadt2[_, Int], """{"$type":"upickle.Gadt2.IsDir","v":123}""")
        test - rw(Gadt2.ReadBytes('h'), """{"$type":"upickle.Gadt2.ReadBytes","v":"h"}""")
//        test - rw(Gadt2.ReadBytes('h'): Gadt2[_, Char], """{"$type":"upickle.Gadt2.ReadBytes","v":"h"}""")
        test - rw(Gadt2.CopyOver(Seq(1, 2, 3), ""), """{"$type":"upickle.Gadt2.CopyOver","src":[1,2,3],"v":""}""")
//        test - rw(Gadt2.CopyOver(Seq(1, 2, 3), ""): Gadt2[_, Unit], """{"$type":"upickle.Gadt2.CopyOver","src":[1,2,3],"v":""}""")
      }
    }
    test("issues"){
      test("issue95"){
        rw(
          Tuple1(List(C1("hello", List("world")))),
          """[[{"name": "hello", "types": ["world"]}]]"""
        )
        rw(
          C2(List(C1("hello", List("world")))),
          """{"results": [{"name": "hello", "types": ["world"]}]}"""
        )

        rw(
          GeoCoding2(List(Result2("a", "b", List("c"))), "d"),
          """{"results": [{"name": "a", "whatever": "b", "types": ["c"]}], "status": "d"}"""
        )
      }
      test("scalatex"){
        val block = Ast.Block(1, Seq(Ast.Block.Text(2, "hello")))
        val blockText = """{
            "$type":"upickle.Ast.Block",
            "offset":1,
            "parts":[
              {
                "$type": "upickle.Ast.Block.Text",
                "offset":2,
                "txt":"hello"
              }
            ]
          }"""
        rw(block: Ast, blockText)
        rw(block: Ast.Block, blockText)
        rw(block: Ast.Block.Sub, blockText)
        rw(block: Ast.Chain.Sub, blockText)

        val header = Ast.Header(0, "Hello", block)
        val headerText = s"""{
          "$$type": "upickle.Ast.Header",
          "offset": 0,
          "front": "Hello",
          "block": $blockText
        }"""
        rw(header: Ast, headerText)
        rw(header: Ast.Header, headerText)
        rw(header: Ast.Block.Sub, headerText)
        rw(header: Ast.Chain.Sub, headerText)
      }

      test("scala-issue-11768"){
        // Make sure this compiles
        new Thing[Int, String](None)
      }
      //      test("companionImplicitPickedUp"){
      //        assert(implicitly[upickle.default.Reader[TypedFoo]] eq TypedFoo.readWriter)
      //        assert(implicitly[upickle.default.Writer[TypedFoo]] eq TypedFoo.readWriter)
      //        assert(implicitly[upickle.default.ReadWriter[TypedFoo]] eq TypedFoo.readWriter)
      //      }
      //      test("companionImplicitWorks"){
      //
      //        rw(TypedFoo.Bar(1): TypedFoo, """{"$type": "upickle.TypedFoo.Bar", "i": 1}""")
      //        rw(TypedFoo.Baz("lol"): TypedFoo, """{"$type": "upickle.TypedFoo.Baz", "s": "lol"}""")
      //        rw(TypedFoo.Quz(true): TypedFoo, """{"$type": "upickle.TypedFoo.Quz", "b": true}""")
      //      }

      test("issue-371") {
        val input = """{"head":"a","tail":[{"head":"b","tail":[]}]}"""
        val expected = Node371("a", Some(Node371("b", None)))
        val result = upickle.default.read[Node371](input)
        assert(result == expected)
      }

      test("hash-collision") {
        def zeroHashCodeStrings: Iterator[String] = {
          def charAndHash(h: Int): Iterator[(Char, Int)] = ('!' to '~').iterator.map(ch => (ch, (h + ch) * 31))

          for {
            (ch0, h0) <- charAndHash(0)
            (ch1, h1) <- charAndHash(h0)
            (ch2, h2) <- charAndHash(h1) if ((h2 + 32) * 923521 ^ (h2 + 127) * 923521) < 0
            (ch3, h3) <- charAndHash(h2) if ((h3 + 32) * 29791 ^ (h3 + 127) * 29791) < 0
            (ch4, h4) <- charAndHash(h3) if ((h4 + 32) * 961 ^ (h4 + 127) * 961) < 0
            (ch5, h5) <- charAndHash(h4) if ((h5 + 32) * 31 ^ (h5 + 127) * 31) < 0
            (ch6, h6) <- charAndHash(h5) if (h6 + 32 ^ h6 + 127) < 0
            (ch7, _) <- charAndHash(h6) if h6 + ch7 == 0
          } yield new String(Array(ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7))
        }

        val jsonString =
          zeroHashCodeStrings
            .map(s => ujson.write(s))
            .take(1000000)
            .mkString("{", s":null,", ":null}")

        test("issue-416") {
          upickle.default.read[Foo416](jsonString)
          ()
        }
        test("issue-446") {
          sys.props("java.vm.name") match {
            case "Scala.js" | "Scala Native" =>
            // The fix assumes a hash collision safe java.util.LinkedHashMap
            // implementation. When/if other platforms will have such characteristics
            // ujson doesn't need to change.
            case _ =>
              ujson.read(jsonString)
          }
          ()
        }
      }
    }
  }

  case class Node371(head: String, tail: Option[Node371])

  object Node371 {
    implicit val nodeRW: ReadWriter[Node371] = macroRW[Node371]
  }

  case class Foo416()
  object Foo416 {
    implicit val rw: ReadWriter[Foo416] = macroRW[Foo416]
  }
  class Thing[T: upickle.default.Writer, V: upickle.default.Writer](t: Option[(V, T)]) {
    implicitly[upickle.default.Writer[Option[(V, T)]]]
  }
}

