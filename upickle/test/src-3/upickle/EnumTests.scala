package upickle

import ujson.ParseException
import upickle.TestUtil.rw
import upickle.core.AbortException

import scala.language.implicitConversions
import utest.{assert, intercept, *}
import upickle.default.*

enum SimpleEnum derives ReadWriter:
  case A, B

enum ColorEnum(val rgb: Int) derives ReadWriter:
  case Red extends ColorEnum(0xFF0000)
  case Green extends ColorEnum(0x00FF00)
  case Blue extends ColorEnum(0x0000FF)
  case Mix(mix: Int) extends ColorEnum(mix)
  case Unknown(mix: Int) extends ColorEnum(0x000000)

case class Enclosing(str: String, simple1: SimpleEnum, simple2: Option[SimpleEnum]) derives ReadWriter

enum LinkedList[+T] derives ReadWriter:
  case End
  case Node(value: T, next: LinkedList[T]) // direct recursion
  case Node2(value: T, next: Node[T]) // indirect recursion

object EnumTests extends TestSuite {


  val tests = Tests {
    test("simple") {
      test("readwrite") - {
        rw[SimpleEnum](SimpleEnum.A, "\"A\"", """{"$type": "A"}""")
        rw[SimpleEnum](SimpleEnum.B, "\"B\"", """{"$type": "B"}""")
//        rw[SimpleEnum.A.type](SimpleEnum.A, "\"A\"", """{"$type": "A"}""")
//        rw[SimpleEnum.B.type](SimpleEnum.B, "\"B\"", """{"$type": "B"}""")
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
      rw[ColorEnum](ColorEnum.Red, "\"Red\"", """{"$type": "Red"}""")
      rw[ColorEnum](ColorEnum.Green, "\"Green\"", """{"$type": "Green"}""")
      rw[ColorEnum](ColorEnum.Blue, "\"Blue\"", """{"$type": "Blue"}""")
      rw[ColorEnum](ColorEnum.Mix(12345), "{\"$type\":\"Mix\",\"mix\":12345}")
      rw[ColorEnum](ColorEnum.Unknown(12345), "{\"$type\":\"Unknown\",\"mix\":12345}")

//      rw[ColorEnum.Red.type](ColorEnum.Red, "\"Red\"", """{"$type": "Red"}""")
//      rw[ColorEnum.Green.type](ColorEnum.Green, "\"Green\"", """{"$type": "Green"}""")
//      rw[ColorEnum.Blue.type](ColorEnum.Blue, "\"Blue\"", """{"$type": "Blue"}""")
//      rw[ColorEnum.Mix](ColorEnum.Mix(12345), "{\"$type\":\"Mix\",\"mix\":12345}")
//      rw[ColorEnum.Unknown](ColorEnum.Unknown(12345), "{\"$type\":\"Unknown\",\"mix\":12345}")
    }

    test("recursive"){
      rw[LinkedList[Int]](LinkedList.End, "\"End\"", """{"$type": "End"}""")
      rw[LinkedList[Int]](
        LinkedList.Node(1, LinkedList.End),
        """{"$type": "Node", "value": 1, "next": "End"}"""
      )
      rw[LinkedList[Int]](
        LinkedList.Node2(1, LinkedList.Node(2, LinkedList.End)),
        """{"$type": "Node2", "value": 1, "next": {"$type": "Node", "value": 2, "next": "End"}}"""
      )
    }
  }
}

