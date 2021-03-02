package upickle

import utest._
import upickle.legacy.read
import ujson.{IncompleteParseException, ParseException}
import upickle.core.AbortException
case class Fee(i: Int, s: String)
object Fee{
  implicit def rw: upickle.legacy.ReadWriter[Fee] = upickle.legacy.macroRW
}
sealed trait Fi
object Fi{
  implicit def rw1: upickle.legacy.ReadWriter[Fi] = upickle.legacy.ReadWriter.merge(Fo.rw1, Fum.rw1)
  implicit def rw2: upickle.default.ReadWriter[Fi] = upickle.default.ReadWriter.merge(Fo.rw2, Fum.rw2)
  case class Fo(i: Int) extends Fi
  object Fo{
    implicit def rw1: upickle.legacy.ReadWriter[Fo] = upickle.legacy.macroRW
    implicit def rw2: upickle.default.ReadWriter[Fo] = upickle.default.macroRW
  }
  case class Fum(s: String) extends Fi
  object Fum{
    implicit def rw1: upickle.legacy.ReadWriter[Fum] = upickle.legacy.macroRW
    implicit def rw2: upickle.default.ReadWriter[Fum] = upickle.default.macroRW
  }
}
/**
* Generally, every failure should be a Invalid.Json or a
* InvalidData. If any assertion errors, match errors, number
* format errors or similar leak through, we've failed
*/
object FailureTests extends TestSuite {

  def tests = Tests {
//    test("test"){
//      read[ujson.Value](""" {unquoted_key: "keys must be quoted"} """)
//    }

    test("jsonFailures"){
      // Run through the test cases from the json.org validation suite,
      // skipping the ones which we don't support yet (e.g. leading zeroes,
      // extra commas) or will never support (e.g. too deep)
      def ensureFailure(failureCase: String) = {
        intercept[ParseException] { read[ujson.Value](failureCase.trim) }
      }

//        test - ensureFailure(""" "A JSON payload should be an object or array, not a string." """)
      test - ensureFailure(""" {"Extra value after close": true} "misplaced quoted value" """)
      test - ensureFailure(""" {"Illegal expression": 1 + 2} """)
      test - ensureFailure(""" {"Illegal invocation": alert()} """)
      test - ensureFailure(""" {"Numbers cannot have leading zeroes": 013} """)
      test - ensureFailure(""" {"Numbers cannot be hex": 0x14} """)
      test - ensureFailure(""" ["Illegal backslash escape: \x15"] """)
      test - ensureFailure(""" [\naked] """)
      test - ensureFailure(""" ["Illegal backslash escape: \017"] """)
//        test - ensureFailure(""" [[[[[[[[[[[[[[[[[[[["Too deep"]]]]]]]]]]]]]]]]]]]] """)
      test - ensureFailure(""" {"Missing colon" null} """)

      test - ensureFailure(""" {"Double colon":: null} """)
      test - ensureFailure(""" {"Comma instead of colon", null} """)
      test - ensureFailure(""" ["Colon instead of comma": false] """)
      test - ensureFailure(""" ["Bad value", truth] """)
      test - ensureFailure(""" ['single quote'] """)
      test - ensureFailure(""" ["	tab	character	in	string	"] """)
      test - ensureFailure(""" ["tab\   character\   in\  string\  "] """)
      test - ensureFailure(""" ["line
        break"] """)
      test - ensureFailure(""" ["line\
        break"] """)
      test - ensureFailure(""" [0e] """)
      test - ensureFailure(""" {unquoted_key: "keys must be quoted"} """)
      test - ensureFailure(""" [0e+-1] """)

      test - ensureFailure(""" ["mismatch"} """)
      test - ensureFailure(""" ["extra comma",] """)
      test - ensureFailure(""" ["double extra comma",,] """)
      test - ensureFailure(""" [   , "<-- missing value"] """)
      test - ensureFailure(""" ["Comma after the close"], """)
      test - ensureFailure(""" ["Extra close"]] """)
      test - ensureFailure(""" {"Extra comma": true,} """)

      test - intercept[IncompleteParseException]{read[ujson.Value](""" {"Comma instead if closing brace": true, """)}
      test - intercept[IncompleteParseException]{read[ujson.Value](""" ["Unclosed array" """)}
    }

    test("facadeFailures"){
      def assertErrorMsg[T: upickle.legacy.Reader](s: String, msgs: String*) = {
        val err = intercept[AbortException] { println("ZZZ " + upickle.legacy.read[T](s)) }
        for (msg <- msgs) assert(err.getMessage.contains(msg))

        err
      }
      def assertErrorMsgDefault[T: upickle.default.Reader](s: String, msgs: String*) = {
        val err = intercept[AbortException] { upickle.default.read[T](s) }
        for (msg <- msgs) assert(err.getMessage.contains(msg))
        err
      }
      test("structs"){
        test - assertErrorMsg[Boolean]("\"lol\"", "expected boolean got string at index 0")
        test - assertErrorMsg[Int]("\"lol\"", "expected number got string at index 0")
        test - assertErrorMsg[Seq[Int]]("\"lol\"", "expected sequence got string at index 0")
        test - assertErrorMsg[Seq[String]]("""["1", 2, 3]""", "expected string got int32 at index 6")
        test("tupleShort"){
          assertErrorMsg[Seq[(Int, String)]](
            "[[1, \"1\"], [2, \"2\"], []]",
            "expected 2 items in sequence, found 0 at index 22"
          )
        }
      }
      test("caseClass"){
        // Separate this guy out because the read macro and
        // the intercept macro play badly with each other
        test("missingKey"){
          test - assertErrorMsg[Fee]("""{"i": 123}""", "missing keys in dictionary: s at index 9")
          test - assertErrorMsg[Fee](""" {"s": "123"}""", "missing keys in dictionary: i at index 1")
          test - assertErrorMsg[Fee]("""  {}""", "missing keys in dictionary: i, s at index 3")
        }
        test("badKey"){
          test - assertErrorMsg[Fee]("""{"i": true}""", "expected number got boolean")
        }

        test("wrongType"){
          test - assertErrorMsg[Fee]("""[1, 2, 3]""", "expected dictionary got sequence at index 0")
          test - assertErrorMsg[Fee]("""31337""", "expected dictionary got int32 at index 0")
        }

        test("invalidTag"){
          test - assertErrorMsg[Fi.Fo]("""["omg", {}]""", "invalid tag for tagged object: omg at index 1")
          test - assertErrorMsg[Fi]("""["omg", {}]""", "invalid tag for tagged object: omg at index 1")
          test - assertErrorMsgDefault[Fi.Fo]("""{"$type": "omg"}]""", "invalid tag for tagged object: omg at index 1")
          test - assertErrorMsgDefault[Fi]("""{"$type": "omg"}]""", "invalid tag for tagged object: omg at index 1")
        }

        test("taggedInvalidBody"){
          test - assertErrorMsg[Fi.Fo]("""["upickle.Fi.Fo", {"i": true, "z": null}]""", "expected number got boolean at index 24")
          test - assertErrorMsg[Fi]("""["upickle.Fi.Fo", {"i": true, "z": null}]""", "expected number got boolean at index 24")
          test - assertErrorMsgDefault[Fi.Fo]("""{"$type": "upickle.Fi.Fo", "i": true, "z": null}""", "expected number got boolean at index 32")
          test - assertErrorMsgDefault[Fi]("""{"$type": "upickle.Fi.Fo", "i": true, "z": null}""", "expected number got boolean at index 32")
        }
      }
    }
    test("compileErrors"){
      compileError("write(new Object)")
      compileError("""read[Object]("")""")
//      compileError("""read[Array[Object]]("")""").msg
      // Make sure this doesn't hang the compiler =/
      compileError("implicitly[upickle.default.Reader[Nothing]]")
    }
    test("expWholeNumbers"){
      upickle.default.read[Byte]("0e0") ==> 0.toByte
      upickle.default.read[Short]("0e0") ==> 0
      upickle.default.read[Char]("0e0") ==> 0.toChar
      upickle.default.read[Int]("0e0") ==> 0
      upickle.default.read[Long]("0e0") ==> 0

      upickle.default.read[Byte]("10e1") ==> 100
      upickle.default.read[Short]("10e1") ==> 100
      upickle.default.read[Char]("10e1") ==> 100
      upickle.default.read[Int]("10e1") ==> 100
      upickle.default.read[Long]("10e1") ==> 100

      upickle.default.read[Byte]("10.1e1") ==> 101
      upickle.default.read[Short]("10.1e1") ==> 101
      upickle.default.read[Char]("10.1e1") ==> 101
      upickle.default.read[Int]("10.1e1") ==> 101
      upickle.default.read[Long]("10.1e1") ==> 101

      // Not supporting these for now, since AFAIK none of the
      // JSON serializers I know generate numbers of this form
//      upickle.default.read[Byte]("10e-1") ==> 1
//      upickle.default.read[Short]("10e-1") ==> 1
//      upickle.default.read[Char]("10e-1") ==> 1
//      upickle.default.read[Int]("10e-1") ==> 1
//      upickle.default.read[Long]("10e-1") ==> 1
    }
  }
}
