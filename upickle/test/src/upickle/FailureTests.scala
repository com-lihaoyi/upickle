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

sealed trait WrongTag

object WrongTag {
  case object Foo1 extends WrongTag

  case object Foo2 extends WrongTag
  implicit val rw1: upickle.default.ReadWriter[Foo1.type] = upickle.default.macroRW
  implicit val rw2: upickle.default.ReadWriter[Foo2.type] = upickle.default.macroRW
  implicit val rw: upickle.default.ReadWriter[WrongTag] = upickle.default.macroRW


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
      def check(failureCase: String, expectedMessage: String) = {
        val ex = intercept[ParseException] { read[ujson.Value](failureCase) }
        assert(ex.getMessage == expectedMessage)
      }
//        """ "A JSON payload should be an object or array, not a string." """,
      test - check(
        """ {"Extra value after close": true} "misplaced quoted value" """,
        """expected whitespace or eof got "\"" at index 35"""
      )
      test - check(
        """ {"Illegal expression": 1 + 2} """,
        """expected , or } got "+" at index 26"""
      )
      test - check(
        """ {"Illegal invocation": alert()} """,
       """expected json value got "a" at index 24"""
      )
      test - check(
        """ {"Numbers cannot have leading zeroes": 013} """,
        """expected , or } got "1" at index 41"""
      )
      test - check(
        """ {"Numbers cannot be hex": 0x14} """,
        """expected , or } got "x" at index 28"""
      )
      test - check(
        """ ["Illegal backslash escape: \x15"] """,
        """illegal escape sequence after \ got "x" at index 30"""
      )
      test - check(
        """ [\naked] """,
        """expected json value or ] got "\\" at index 2"""
      )
      test - check(
        """ ["Illegal backslash escape: \017"] """,
        """illegal escape sequence after \ got "0" at index 30"""
      )
//        """ [[[[[[[[[[[[[[[[[[[["Too deep"]]]]]]]]]]]]]]]]]]]] """,
      test - check(
        """ {"Missing colon" null} """,
        """expected : got "n" at index 18"""
      )

      test - check(
        """ {"Double colon":: null} """,
        """expected json value got ":" at index 17"""
      )
      test - check(
        """ {"Comma instead of colon", null} """,
        """expected : got "," at index 26"""
      )
      test - check(
        """ ["Colon instead of comma": false] """,
        """expected , or ] got ":" at index 26"""
      )
      test - check(
        """ ["Bad value", truth] """,
        """expected true got "t" at index 15"""
      )
      test - check(
        """ ['single quote'] """,
        """expected json value or ] got "'" at index 2"""
      )
      test - check(
        """ ["	tab	character	in	string	"] """,
        """control char (9) in string got "\t" at index 3"""
      )
      test - check(
        """ ["tab\   character\   in\  string\  "] """,
        """illegal escape sequence after \ got " " at index 7"""
      )
      test - check(
        """ ["line
          break"] """,
        """control char (10) in string got "\n" at index 7"""
      )
      test - check(
        """ ["line\
          break"] """,
        """illegal escape sequence after \ got "\n" at index 8"""
      )
      test - check(
        """ [0e] """,
        """expected digit got "0" at index 2"""
      )
      test - check(
        """ {unquoted_key: "keys must be quoted"} """,
        """expected json value or } got "u" at index 2"""
      )
      test - check(
        """ [0e+-1] """,
        """expected digit got "0" at index 2"""
      )

      test - check(
        """ ["mismatch"} """,
        """expected , or ] got "}" at index 12"""
      )
      test - check(
        """ ["extra comma",] """,
        """expected json value got "]" at index 16"""
      )
      test - check(
        """ ["double extra comma",,] """,
        """expected json value got "," at index 23"""
      )
      test - check(
        """ [   , "<-- missing value"] """,
        """expected json value or ] got "," at index 5"""
      )
      test - check(
        """ ["Comma after the close"], """,
        """expected whitespace or eof got "," at index 26"""
      )
      test - check(
        """ ["Extra close"]] """,
        """expected whitespace or eof got "]" at index 16"""
      )
      test - check(
      """ {"Extra comma": true,} """,
        """expected json string key got "}" at index 22"""
      )
      test{ intercept[IncompleteParseException]{read[ujson.Value](""" {"Comma instead if closing brace": true, """)} }
      test{ intercept[IncompleteParseException]{read[ujson.Value](""" ["Unclosed array" """)} }
    }

    test("facadeFailures"){
      def assertErrorMsg[T: upickle.legacy.Reader](s: String, msgs: String*) = {
        val err = intercept[Exception] { upickle.legacy.read[T](s) }
        for (msg <- msgs) assert(err.getMessage.contains(msg))

        err
      }

      def assertErrorMsgDefault[T: upickle.default.Reader](s: String, msgs: String*) = {
        val err = intercept[AbortException] { upickle.default.read[T](s) }
        for (msg <- msgs) assert(err.getMessage.contains(msg))
        err
      }

      test("structs"){
        test - assertErrorMsg[Int]("\"lol\"", "lol")
        test - assertErrorMsg[Seq[Int]]("\"lol\"", "expected sequence got string at index 0")
        test - assertErrorMsg[Seq[String]]("""["1", [2], [3]]""", "expected string got sequence at index 6")
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
          test - assertErrorMsg[Fee]("""31337""", "expected dictionary got number at index 0")
        }

        test("invalidTag"){
          test - assertErrorMsg[Fi.Fo]("""["omg", {}]""", "invalid tag for tagged object: omg at index 1")
          test - assertErrorMsg[Fi]("""["omg", {}]""", "invalid tag for tagged object: omg at index 1")
          test - assertErrorMsgDefault[Fi.Fo]("""{"$type": "omg"}""", "invalid tag for tagged object: omg at index 1")
          test - assertErrorMsgDefault[Fi](""""omg"""", "invalid tag for tagged object: omg at index 0")
          test - assertErrorMsgDefault[Fi]("""{"$type": "omg"}""", "invalid tag for tagged object: omg at index 1")
          test - assertErrorMsgDefault[Fi]("""{}""", """Missing key "$type" for tagged dictionary at index 1""")
          test - assertErrorMsgDefault[Fi]("""{"mispelledTypeTag": "omg"}""", """Missing key "$type" for tagged dictionary at index 26""")
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
