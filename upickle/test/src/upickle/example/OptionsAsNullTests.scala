package upickle.example

import utest._
import upickle.example.Simple.Thing
import scala.language.implicitConversions

case class Opt(a: Option[String], b: Option[Int])
object Opt{
  implicit def rw: OptionPickler.ReadWriter[Opt] = OptionPickler.macroRW
}
object OptionPickler extends upickle.AttributeTagged {
  override implicit def OptionWriter[T: Writer]: Writer[Option[T]] =
    implicitly[Writer[T]].comap[Option[T]] {
      case None => null.asInstanceOf[T]
      case Some(x) => x
    }

  override implicit def OptionReader[T: Reader]: Reader[Option[T]] = {
    new Reader.Delegate[Any, Option[T]](implicitly[Reader[T]].map(Some(_))){
      override def visitNull(index: Int) = None
    }
  }
}
// end_ex

object OptionsAsNullTests extends TestSuite {

  import OptionPickler._
  implicit def rw: OptionPickler.ReadWriter[Thing] = OptionPickler.macroRW
  val tests = TestSuite {
    test("nullAsNone"){

      // Quick check to ensure we didn't break anything
      test("primitive"){
        write("A String") ==> "\"A String\""
        read[String]("\"A String\"") ==> "A String"
        write(1) ==> "1"
        read[Int]("1") ==> 1
        write(Thing(1, "gg")) ==> """{"myFieldA":1,"myFieldB":"gg"}"""
        read[Thing]("""{"myFieldA":1,"myFieldB":"gg"}""") ==> Thing(1, "gg")
      }

      test("none"){
        write[None.type](None) ==> "null"
        read[None.type]("null") ==> None
      }

      test("some"){
        write(Some("abc")) ==> "\"abc\""
        read[Some[String]]("\"abc\"") ==> Some("abc")
        write(Some(1)) ==> "1"
        read[Some[Int]]("1") ==> Some(1)
        write(Some(3.14159)) ==> "3.14159"
        read[Some[Double]]("3.14159") ==> Some(3.14159)
      }

      test("option"){
        write(Option("abc")) ==> "\"abc\""
        read[Option[String]]("\"abc\"") ==> Some("abc")
        read[Option[String]]("null") ==> None
      }

      test("caseClass"){
        write(Opt(None, None)) ==> """{"a":null,"b":null}"""
        read[Opt]("""{"a":null,"b":null}""") ==> Opt(None, None)
        write(Opt(Some("abc"), Some(1))) ==> """{"a":"abc","b":1}"""
      }

      test("optionCaseClass"){
        write(Opt(None, None)) ==> """{"a":null,"b":null}"""
        read[Opt]("""{"a":null,"b":null}""") ==> Opt(None, None)
        write(Opt(Some("abc"), Some(1))) ==> """{"a":"abc","b":1}"""

        write(Option(Thing(1, "gg"))) ==> """{"myFieldA":1,"myFieldB":"gg"}"""
        read[Option[Thing]]("""{"myFieldA":1,"myFieldB":"gg"}""") ==> Option(Thing(1, "gg"))
      }

      // New tests.  Work as expected.
      test("customPickler") {
        // Custom pickler copied from the documentation
        class CustomThing2(val i: Int, val s: String)

        object CustomThing2 {
          implicit val rw: OptionPickler.ReadWriter[CustomThing2] = /*upickle.default*/ OptionPickler.readwriter[String].bimap[CustomThing2](
            x => s"${x.i} ${x.s}",
            str => {
              val Array(i, s) = str.split(" ", 2)
              new CustomThing2(i.toInt, s)
            }
          )
        }

        test("customClass") {
          write(new CustomThing2(10, "Custom")) ==> "\"10 Custom\""
          val r = read[CustomThing2]("\"10 Custom\"")
          assert(r.i == 10, r.s == "Custom")
        }

        test("optCustomClass_Some") {
          write(Some(new CustomThing2(10, "Custom"))) ==> "\"10 Custom\""
          val r = read[Option[CustomThing2]]("\"10 Custom\"")
          assert(r.get.i == 10, r.get.s == "Custom")
        }

        test("optCustomClass_None") {
          read[Option[CustomThing2]]("null") ==> None
        }

      }

      // Copied from ExampleTests
      test("Js") {
        import OptionPickler._   // changed from upickle.default._
        case class Bar(i: Int, s: String)
        implicit val fooReadWrite: ReadWriter[Bar] =
          readwriter[ujson.Value].bimap[Bar](
            x => ujson.Arr(x.s, x.i),
            json => new Bar(json(1).num.toInt, json(0).str)
          )

        write(Bar(123, "abc")) ==> """["abc",123]"""
        read[Bar]("""["abc",123]""") ==> Bar(123, "abc")

        // New tests.  Last one fails.  Why?
        test("option") {
          test("write") {write(Some(Bar(123, "abc"))) ==> """["abc",123]"""}
          test("readSome") {read[Option[Bar]]("""["abc",123]""") ==> Some(Bar(123, "abc"))}
          test("readNull") {read[Option[Bar]]("""null""") ==> None}
        }
      }

    }
  }
}
