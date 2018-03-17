package upickle

import utest._
import upickle.legacy.read
import acyclic.file
import upickle.jawn.{FacadeException, IncompleteParseException, ParseException}
case class Fee(i: Int, s: String)
object Fee{
  implicit def rw: upickle.legacy.ReadWriter[Fee] = upickle.legacy.macroRW
}
sealed trait Fi
object Fi{
  implicit def rw: upickle.legacy.ReadWriter[Fi] = upickle.legacy.ReadWriter.merge(Fo.rw, Fum.rw)
  case class Fo(i: Int) extends Fi
  object Fo{
    implicit def rw: upickle.legacy.ReadWriter[Fo] = upickle.legacy.macroRW
  }
  case class Fum(s: String) extends Fi
  object Fum{
    implicit def rw: upickle.legacy.ReadWriter[Fum] = upickle.legacy.macroRW
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
    'jsonFailures {
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
    'otherFailures{
      'nonMacroFailures{
        * - {
          val err = intercept[FacadeException] { read[Boolean]("\"lol\"") }
//          assert(err.msg.contains("expected boolean"))
          err
        }
        * - {
          val err = intercept[FacadeException] { read[Int]("\"lol\"") }
          assert(err.msg.contains("expected number got string"))
          err
        }
        * - {
          val err = intercept[FacadeException] { read[Seq[Int]]("\"lol\"") }
          assert(err.msg.contains("expected sequence got string"))
          err
        }
        * - {
          val err = intercept[FacadeException] { read[Seq[String]]("[1, 2, 3]") }
          assert(err.msg.contains("expected string got number"))
          err
        }
        'tupleShort - {
          val err = intercept[FacadeException] { read[Seq[(Int, String)]]("[[1, \"1\"], [2, \"2\"], []]") }
          assert(err.msg.contains("expected 2 items in sequence"))
          err
        }
      }
      'macroFailures{
        // Separate this guy out because the read macro and
        // the intercept macro play badly with each other
        'missingKey {
          * - {
            val readFoo = () => read[Fee]( """{"i": 123}""")
            val err = intercept[FacadeException]{ readFoo() }
            assert(err.msg.contains("missing keys in dictionary: s"))
            err
          }
          * - {
            val readFoo = () => read[Fee]( """{}""")
            val err = intercept[FacadeException]{ readFoo() }
            assert(err.msg.contains("missing keys in dictionary: i, s"))
            err
          }
        }
        'completelyInvalid{
          val readFoo2 = () => read[Fee]("""[1, 2, 3]""")
          val err = intercept[FacadeException] { readFoo2() }
          assert(err.msg.contains("dictionary"))
          err
        }

        'invalidTag {
          val readFo = () => read[Fi.Fo]( """["omg", {}]""")
          val err = intercept[FacadeException]{ readFo() }
//          assert(err.msg.contains("Tagged Object"))
//          assert(err.msg.contains("upickle.Fi.Fo"))
          err
        }

        'invalidTag2{
          val readFo2 = () => read[Fi]("""["omg", {}]""")
          val err = intercept[FacadeException]{ readFo2() }
//          assert(err.msg.contains("Tagged Object"))
//          assert(err.msg.contains("upickle.Fi"))
          err
        }
      }
    }
    'compileErrors{
      compileError("write(new Object)")
      compileError("""read[Object]("")""")
//      compileError("""read[Array[Object]]("")""").msg
      // Make sure this doesn't hang the compiler =/
      compileError("implicitly[upickle.default.Reader[Nothing]]")
    }
  }
}
