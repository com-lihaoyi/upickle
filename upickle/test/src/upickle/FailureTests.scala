package upickle

import utest._
import upickle.legacy.read
import acyclic.file
import ujson.{IncompleteParseException, JsonProcessingException, ParseException}
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
//    'test - {
//      read[Js.Value](""" {unquoted_key: "keys must be quoted"} """)
//    }

    'jsonFailures - {
      // Run through the test cases from the json.org validation suite,
      // skipping the ones which we don't support yet (e.g. leading zeroes,
      // extra commas) or will never support (e.g. too deep)

      val failureCases = Seq(
//        """ "A JSON payload should be an object or array, not a string." """,
        """ {"Extra value after close": true} "misplaced quoted value" """,
        """ {"Illegal expression": 1 + 2} """,
        """ {"Illegal invocation": alert()} """,
        """ {"Numbers cannot have leading zeroes": 013} """,
        """ {"Numbers cannot be hex": 0x14} """,
        """ ["Illegal backslash escape: \x15"] """,
        """ [\naked] """,
        """ ["Illegal backslash escape: \017"] """,
//        """ [[[[[[[[[[[[[[[[[[[["Too deep"]]]]]]]]]]]]]]]]]]]] """,
        """ {"Missing colon" null} """,

        """ {"Double colon":: null} """,
        """ {"Comma instead of colon", null} """,
        """ ["Colon instead of comma": false] """,
        """ ["Bad value", truth] """,
        """ ['single quote'] """,
        """ ["	tab	character	in	string	"] """,
        """ ["tab\   character\   in\  string\  "] """,
        """ ["line
          break"] """,
        """ ["line\
          break"] """,
        """ [0e] """,
        """ {unquoted_key: "keys must be quoted"} """,
        """ [0e+-1] """,

        """ ["mismatch"} """,
        """ ["extra comma",] """,
        """ ["double extra comma",,] """,
        """ [   , "<-- missing value"] """,
        """ ["Comma after the close"], """,
        """ ["Extra close"]] """,
        """ {"Extra comma": true,} """
      ).map(_.trim())
      val res =
        for(failureCase <- failureCases)
        yield try {
          intercept[ParseException] { read[Js.Value](failureCase) }
          None
        }catch{
          case _:Throwable =>
          Some(failureCase)
        }

      val nonFailures = res.flatten
      assert(nonFailures.isEmpty)
      intercept[IncompleteParseException]{read[Js.Value](""" {"Comma instead if closing brace": true, """)}
      intercept[IncompleteParseException]{read[Js.Value](""" ["Unclosed array" """)}
    }

    'facadeFailures - {
      def assertErrorMsg[T: upickle.legacy.Reader](s: String, msgs: String*) = {
        val err = intercept[JsonProcessingException] { upickle.legacy.read[T](s) }
        for (msg <- msgs) assert(err.getMessage.contains(msg))

        err
      }
      def assertErrorMsgDefault[T: upickle.default.Reader](s: String, msgs: String*) = {
        val err = intercept[JsonProcessingException] { upickle.default.read[T](s) }
        for (msg <- msgs) assert(err.getMessage.contains(msg))
        err
      }
      'structs - {
        * - assertErrorMsg[Boolean]("\"lol\"", "expected boolean got string at index 0")
        * - assertErrorMsg[Int]("\"lol\"", "expected number got string at index 0")
        * - assertErrorMsg[Seq[Int]]("\"lol\"", "expected sequence got string at index 0")
        * - assertErrorMsg[Seq[String]]("""["1", 2, 3]""", "expected string got number at index 6")
        'tupleShort - {
          assertErrorMsg[Seq[(Int, String)]](
            "[[1, \"1\"], [2, \"2\"], []]",
            "expected 2 items in sequence, found 0 at index 22"
          )
        }
      }
      'caseClass - {
        // Separate this guy out because the read macro and
        // the intercept macro play badly with each other
        'missingKey - {
          * - assertErrorMsg[Fee]("""{"i": 123}""", "missing keys in dictionary: s at index 9")
          * - assertErrorMsg[Fee](""" {"s": "123"}""", "missing keys in dictionary: i at index 1")
          * - assertErrorMsg[Fee]("""  {}""", "missing keys in dictionary: i, s at index 3")
        }
        'badKey - {
          * - assertErrorMsg[Fee]("""{"i": true}""", "expected number got boolean")
        }

        'wrongType - {
          * - assertErrorMsg[Fee]("""[1, 2, 3]""", "expected dictionary got sequence at index 0")
          * - assertErrorMsg[Fee]("""31337""", "expected dictionary got number at index 0")
        }

        'invalidTag - {
          * - assertErrorMsg[Fi.Fo]("""["omg", {}]""", "invalid tag for tagged object: omg at index 1")
          * - assertErrorMsg[Fi]("""["omg", {}]""", "invalid tag for tagged object: omg at index 1")
          * - assertErrorMsgDefault[Fi.Fo]("""{"$type": "omg"}]""", "invalid tag for tagged object: omg at index 1")
          * - assertErrorMsgDefault[Fi]("""{"$type": "omg"}]""", "invalid tag for tagged object: omg at index 1")
        }

        'taggedInvalidBody - {
          * - assertErrorMsg[Fi.Fo]("""["upickle.Fi.Fo", {"i": true, "z": null}]""", "expected number got boolean at index 24")
          * - assertErrorMsg[Fi]("""["upickle.Fi.Fo", {"i": true, "z": null}]""", "expected number got boolean at index 24")
          * - assertErrorMsgDefault[Fi.Fo]("""{"$type": "upickle.Fi.Fo", "i": true, "z": null}""", "expected number got boolean at index 32")
          * - assertErrorMsgDefault[Fi]("""{"$type": "upickle.Fi.Fo", "i": true, "z": null}""", "expected number got boolean at index 32")
        }
      }
    }
    'compileErrors - {
      compileError("write(new Object)")
      compileError("""read[Object]("")""")
//      compileError("""read[Array[Object]]("")""").msg
      // Make sure this doesn't hang the compiler =/
      compileError("implicitly[upickle.default.Reader[Nothing]]")
    }
  }
}
