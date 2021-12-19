package upickle

import ujson.ParseException
import upickle.core.AbortException

import scala.language.implicitConversions
import utest.{assert, intercept, *}
import upickle.default.*


enum SimpleEnum derives ReadWriter:
  case A, B

case class Enclosing(str: String, simple1: SimpleEnum, simple2: Option[SimpleEnum]) derives ReadWriter

object EnumTests extends TestSuite {
  val tests = Tests {
    test("simple") {
      test("enum write") - {
        assert(write(SimpleEnum.A) == "\"A\"")
        assert(write(SimpleEnum.B) == "\"B\"")
      }

      test("enum read") - {
        assert(read[SimpleEnum]("\"A\"") == SimpleEnum.A)
        assert(read[SimpleEnum]("\"B\"") == SimpleEnum.B)
      }

      test("enum read failure") - {
        val ex = intercept[AbortException] { read[SimpleEnum]("\"C\"") }
        val expectedMessage = "Value 'C' was not found in enumeration SimpleEnum[values: A, B] at index 0"
        assert(ex.getMessage == expectedMessage)
      }

      test("enclosing write") - {
        val written = write(Enclosing("test", SimpleEnum.A, Some(SimpleEnum.B)))
        val expected = """{"str":"test","simple1":"A","simple2":["B"]}"""
        assert(written == expected)
      }
    }
  }
}

