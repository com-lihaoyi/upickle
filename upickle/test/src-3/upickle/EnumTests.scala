package upickle

import ujson.ParseException
import upickle.TestUtil.rw
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

enum ColorEnum(val rgb: Int):
  case Red extends ColorEnum(0xFF0000)
  case Green extends ColorEnum(0x00FF00)
  case Blue extends ColorEnum(0x0000FF)
  case Mix(mix: Int) extends ColorEnum(mix)
  case Unknown(mix: Int) extends ColorEnum(0x000000)

object ColorEnum{
  implicit val rwRed: ReadWriter[ColorEnum.Red.type] = macroRW
  implicit val rwGreen: ReadWriter[ColorEnum.Green.type] = macroRW
  implicit val rwBlue: ReadWriter[ColorEnum.Blue.type] = macroRW
  implicit val rwMix: ReadWriter[ColorEnum.Mix] = macroRW
  implicit val rwUnknown: ReadWriter[ColorEnum.Unknown] = macroRW
  implicit val rwColor: ReadWriter[ColorEnum] = macroRW
}

case class Enclosing(str: String, simple1: SimpleEnum, simple2: Option[SimpleEnum]) derives ReadWriter

object EnumTests extends TestSuite {

  val tests = Tests {
    test("simple") {
      test("readwrite") - {
        rw[SimpleEnum](SimpleEnum.A, "\"A\"")
        rw[SimpleEnum](SimpleEnum.B, "\"B\"")
        rw[SimpleEnum.A.type](SimpleEnum.A, "\"A\"")
        rw[SimpleEnum.B.type](SimpleEnum.B, "\"B\"")
      }

      test("readFailure") - {
        val ex = intercept[AbortException] { read[SimpleEnum]("\"C\"") }
        val expectedMessage = "invalid tag for tagged object: C at index 0"
        assert(ex.getMessage == expectedMessage)
      }

      test("enclosingWrite") - {
        rw(
          Enclosing("test", SimpleEnum.A, Some(SimpleEnum.B)),
          """{"str":"test","simple1":"A","simple2":["B"]}"""
        )
      }
    }
    test("color"){
      rw[ColorEnum](ColorEnum.Red, "\"Red\"")
      rw[ColorEnum](ColorEnum.Green, "\"Green\"")
      rw[ColorEnum](ColorEnum.Blue, "\"Blue\"")
      rw[ColorEnum](ColorEnum.Mix(12345), "{\"$type\":\"Mix\",\"mix\":12345}")
      rw[ColorEnum](ColorEnum.Unknown(12345), "{\"$type\":\"Unknown\",\"mix\":12345}")

      rw[ColorEnum.Red.type](ColorEnum.Red, "\"Red\"")
      rw[ColorEnum.Green.type](ColorEnum.Green, "\"Green\"")
      rw[ColorEnum.Blue.type](ColorEnum.Blue, "\"Blue\"")
      rw[ColorEnum.Mix](ColorEnum.Mix(12345), "{\"$type\":\"Mix\",\"mix\":12345}")
      rw[ColorEnum.Unknown](ColorEnum.Unknown(12345), "{\"$type\":\"Unknown\",\"mix\":12345}")
    }
  }
}

