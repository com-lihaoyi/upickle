package upickle

import upickle.core.JsonPointerVisitor.JsonPointerException
import upickle.core.{JsonPointerVisitor, NoOpVisitor}
import utest._

import scala.annotation.tailrec

/**
  * @see [[upickle.core.JsonPointerVisitor]]
  */
object JsonPointerVisitorTests extends TestSuite {

  case class Foo(foo: List[String], s: String, i: Int, b: Boolean)

  implicit lazy val rw = upickle.default.macroRW[Foo]

  override def tests: Tests = Tests {
    test("failures") {
      def assertPathFailure(json: String, expectedPath: String) = {
        val cause = intercept[Exception](ujson.transform(json, JsonPointerVisitor(rw)))
        val failureAtPath = findException(cause)
        failureAtPath.get.jsonPointer ==> expectedPath
      }

      @tailrec def findException(t: Throwable): Option[JsonPointerException] = {
        t match {
          case jpe: JsonPointerException => Some(jpe)
          case other if other.getCause != t => findException(t)
          case _ => None
        }
      }

      test - assertPathFailure("""666""", "") // yes, empty string. https://tools.ietf.org/html/rfc6901#section-5
      test - assertPathFailure("""{"foo": -666, "s": "", "i": 5, "b": true}""", "/foo")
      test - assertPathFailure("""{"foo": [-666], "s": "", "i": 5, "b": true}""", "/foo/0")
      test - assertPathFailure("""{"foo": ["", -666], "s": "", "i": 5, "b": true}""", "/foo/1")
      test - assertPathFailure("""{"foo": ["", -666, ""], "s": "", "i": 5, "b": true}""", "/foo/1")
      test - assertPathFailure("""{"foo": [], "s": -666, "i": 5, "b": true}""", "/s")
      test - assertPathFailure("""{"foo": [], "s": "", "i": "-666", "b": true}""", "/i")
      test - assertPathFailure("""{"foo": [], "s": "", "i": 5, "b": -666}""", "/b")
      test - assertPathFailure("""{"foo": [], "s": "", "i": 5}""", "")
    }

    test("JSON Pointer rfc6901") {
      /**
        * {{{
        *    {
        *       "foo": ["bar", "baz"],
        *       "": 0,
        *       "a/b": 1,
        *       "c%d": 2,
        *       "e^f": 3,
        *       "g|h": 4,
        *       "i\\j": 5,
        *       "k\"l": 6,
        *       " ": 7,
        *       "m~n": 8
        *    }
        * }}}
        */
      test("compliance") - {
        val visitor = JsonPointerVisitor(NoOpVisitor)
        visitor.toString ==> ""

        val obj = visitor.visitObject(-1, -1)
        obj.toString ==> ""

        obj.visitKey(-1).visitString("foo", -1)
        obj.toString ==> "/foo"
        obj.visitKeyValue(())

        val foo = obj.subVisitor
        foo.toString ==> "/foo"

        val arr = foo.visitArray(-1, -1).narrow
        arr.toString ==> "/foo" // still describes the array, itself.

        val fooBar = arr.subVisitor
        fooBar.toString ==> "/foo/0"
        fooBar.visitString("bar", -1)
        arr.visitValue((), -1)
        fooBar.toString ==> "/foo/0"

        val fooBaz = arr.subVisitor
        fooBaz.toString ==> "/foo/1"
        fooBaz.visitString("baz", -1)
        arr.visitValue((), -1)
        fooBaz.toString ==> "/foo/1"

        arr.visitEnd(-1)
        arr.toString ==> "/foo" // if visitEnd fails, pointing to the array seems more reasonable than /foo/1.
        obj.visitValue((), -1)
        obj.toString ==> ""

        def appendKey(key: String, expectedPath: String) = {
          obj.visitKey(-1).visitString(key, -1)
          obj.toString ==> expectedPath
          obj.visitKeyValue(())
          val sub = obj.subVisitor
          sub.toString ==> expectedPath
          sub.visitInt32(42, -1)
          obj.visitValue((), -1)
        }

        appendKey("""""", """/""")
        appendKey("""a/b""", """/a~1b""")
        appendKey("""c%d""", """/c%d""")
        appendKey("""e^f""", """/e^f""")
        appendKey("""g|h""", """/g|h""")
        appendKey("""i\\j""", """/i\\j""")
        appendKey("""k\"l""", """/k\"l""")
        appendKey(""" """, """/ """)
        appendKey("""m~n""", """/m~0n""")

        obj.visitEnd(-1)
        obj.toString ==> ""
        visitor.toString ==> ""
      }
    }
  }
}
