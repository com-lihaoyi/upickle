package upickle

import upickle.default._
import utest._

object InANamespaceNamedTests extends TestSuite {
  object upickle {
    case class Foo(i: Int)
    object Foo {
      implicit val rw: ReadWriter[Foo] = macroRW[Foo]
    } 
  }
  val tests = Tests {
    test("it should compile when macroRW is called in a namespace named upickle") {
      val result = write(upickle.Foo(1))
      val expected = """{"i":1}"""
      assert(result == expected)
    }
  }
}
