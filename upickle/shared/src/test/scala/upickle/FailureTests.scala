package upickle

import utest._
import upickle.legacy.read
import acyclic.file
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

  def tests = TestSuite {
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
        """ ["Unclosed array" """,
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
        """ {"Comma instead if closing brace": true, """,
        """ ["mismatch"} """,
        """ ["extra comma",] """,
        """ ["double extra comma",,] """,
        """ [   , "<-- missing value"] """,
        """ ["Comma after the close"], """,
        """ ["Extra close"]] """,
        """ {"Extra comma": true,} """
      ).map(_.trim())
      for(failureCase <- failureCases){
//        println(failureCase)
        try {
          intercept[Invalid.Json] {
            read[Int](failureCase)
          }
        }catch{
          case _:Throwable =>
            println("DIDN'T FAIL: " + failureCase)
        }
      }
    }
    'otherFailures{
      'nonMacroFailures{
        intercept[Invalid.Data] { read[Boolean]("\"lol\"") }
        intercept[Invalid.Data] { read[Int]("\"lol\"") }
        intercept[Invalid.Data] { read[Seq[Int]]("\"lol\"") }
        intercept[Invalid.Data] { read[Seq[String]]("[1, 2, 3]") }
        intercept[Invalid.Data] { read[Seq[(Int, String)]]("[[1, \"1\"], [2, \"2\"], []]") }
      }
      'macroFailures{
        // Separate this guy out because the read macro and
        // the intercept macro play badly with each other
        'missingKey {
          val readFoo = () => read[Fee]( """{"i": 123}""")
          val err = intercept[Invalid.Data]{ readFoo() }
          assert(err.msg.contains("Key Missing: s"))
        }
        'completelyInvalid{
          val readFoo2 = () => read[Fee]("""[1, 2, 3]""")
          val err = intercept[Invalid.Data] { readFoo2() }
          assert(err.msg.contains("Object"))
          println(err)
        }

        'invalidTag {
          val readFo = () => read[Fi.Fo]( """["omg", {}]""")
          val err = intercept[Invalid.Data]{ readFo() }
          assert(err.msg.contains("Tagged Object"))
          assert(err.msg.contains("upickle.Fi.Fo"))
        }

        'invalidTag2{
          val readFo2 = () => read[Fi]("""["omg", {}]""")
          val err = intercept[Invalid.Data]{ readFo2() }
          assert(err.msg.contains("Tagged Object"))
          assert(err.msg.contains("upickle.Fi"))
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
