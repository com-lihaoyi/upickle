package upickle

import ujson.ParseException
import upickle.core.AbortException

import scala.language.implicitConversions
import utest.{assert, intercept, *}
import upickle.default.*


enum SimpleEnum derives ReadWriter:
  case A, B

sealed trait FooX derives ReadWriter
object FooX{
  case object Foo1 extends FooX
  case object Foo2 extends FooX

}
case class Enclosing(str: String, simple1: SimpleEnum, simple2: Option[SimpleEnum]) derives ReadWriter

object EnumTests extends TestSuite {
//  enum Color(val rgb: Int):
//    case Red extends Color(0xFF0000)
//    case Green extends Color(0x00FF00)
//    case Blue extends Color(0x0000FF)
//    case Mix(mix: Int) extends Color(mix)
//
//
//  implicit val rwMix: ReadWriter[Color.Mix] = macroRW
//  implicit val rwColor: ReadWriter[Color] = macroRW

  val tests = Tests {
    test("simple") {
      test("write") - {
        assert(write[SimpleEnum](SimpleEnum.A) == "\"A\"")
        assert(write[SimpleEnum](SimpleEnum.B) == "\"B\"")
        assert(write[SimpleEnum.A.type](SimpleEnum.A) == "\"A\"")
        assert(write[SimpleEnum.B.type](SimpleEnum.B) == "\"B\"")
      }

      test("read") - {
        assert(read[SimpleEnum.A.type]("\"A\"") == SimpleEnum.A)
        assert(read[SimpleEnum.B.type]("\"B\"") == SimpleEnum.B)
        assert(read[SimpleEnum]("\"A\"") == SimpleEnum.A)
        assert(read[SimpleEnum]("\"B\"") == SimpleEnum.B)
      }

      test("readFailure2") - {
        read[FooX]("\"C\"")
      }
      test("readFailure") - {
        val ex = intercept[AbortException] { read[SimpleEnum]("\"C\"") }
        val expectedMessage = "Value 'C' was not found in enumeration SimpleEnum[values: A, B] at index 0"
        assert(ex.getMessage == expectedMessage)
      }

      test("enclosingWrite") - {
        val written = write(Enclosing("test", SimpleEnum.A, Some(SimpleEnum.B)))
        val expected = """{"str":"test","simple1":"A","simple2":["B"]}"""
        assert(written == expected)
      }
    }
  }
}

