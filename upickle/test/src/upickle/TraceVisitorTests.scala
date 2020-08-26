package upickle

import upickle.core.TraceVisitor.TraceException
import upickle.core.{TraceVisitor, NoOpVisitor}
import utest._

import scala.annotation.tailrec

/**
  * @see [[upickle.core.TraceVisitor]]
  */
object TraceVisitorTests extends TestSuite {

  case class Foo(foo: List[String], s: String, i: Int, b: Boolean)

  implicit lazy val rw: upickle.default.ReadWriter[Foo] = upickle.default.macroRW[Foo]

  override def tests: Tests = Tests {
    test("failures") {
      def assertPathFailure(json: String, expectedPath: String) = {
        val cause = intercept[Exception](upickle.default.read(json, trace = true))
        val failureAtPath = findException(cause)
        failureAtPath.get.jsonPath ==> expectedPath
      }

      @tailrec def findException(t: Throwable): Option[TraceException] = {
        t match {
          case jpe: TraceException => Some(jpe)
          case other if other.getCause != t => findException(t)
          case _ => None
        }
      }

      test("ujsonFailures") - {
        test - assertPathFailure("""{""", "$")
        test - assertPathFailure("""""", "$")
        test - assertPathFailure("""{"foo""", "$")
        test - assertPathFailure("""{"foo"}""", "$['foo']")
        test - assertPathFailure("""{"foo"}""", "$['foo']")
        test - assertPathFailure("""{"foo": ["a}""", "$['foo'][0]")
        test - assertPathFailure("""{"foo": ["a"}""", "$['foo'][1]")
        test - assertPathFailure("""{"foo": ["a",}""", "$['foo'][1]")
        test - assertPathFailure("""{"foo": ["a"], "s"}""", "$['s']")
        test - assertPathFailure("""{"foo": ["a"], "s":}""", "$['s']")
        test - assertPathFailure("""{"foo": [], "s": "", "i"""", "$['i']")
        test - assertPathFailure("""{"foo": [], "s": "", "i": """, "$['i']")
        test - assertPathFailure("""{"foo": [], "s": "", "i": 5""", "$['i']")
      }
      test("upickleFailures") - {
        test - assertPathFailure("""666""", "$") // yes, empty string. https://tools.ietf.org/html/rfc6901#section-5
        test - assertPathFailure("""{"foo": -666, "s": "", "i": 5, "b": true}""", "$['foo']")
        test - assertPathFailure("""{"foo": [-666], "s": "", "i": 5, "b": true}""", "$['foo'][0]")
        test - assertPathFailure("""{"foo": ["", -666], "s": "", "i": 5, "b": true}""", "$['foo'][1]")
        test - assertPathFailure("""{"foo": ["", -666, ""], "s": "", "i": 5, "b": true}""", "$['foo'][1]")
        test - assertPathFailure("""{"foo": [], "s": -666, "i": 5, "b": true}""", "$['s']")
        test - assertPathFailure("""{"foo": [], "s": "", "i": "-666", "b": true}""", "$['i']")
        test - assertPathFailure("""{"foo": [], "s": "", "i": 5, "b": -666}""", "$['b']")
        test - assertPathFailure("""{"foo": [], "s": "", "i": 5}""", "$")
      }
    }
  }
}
