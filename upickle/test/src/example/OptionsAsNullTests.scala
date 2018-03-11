package example

import acyclic.file
import upickle.{Js, TestUtil}
import utest._
import example.Simple.Thing
import jawn.{FContext, Facade, RawFContext, RawFacade}

case class Opt(a: Option[String], b: Option[Int])
object Opt{
  implicit def rw: OptionPickler.ReadWriter[Opt] = OptionPickler.macroRW
}
object OptionPickler extends upickle.AttributeTagged {
  override implicit def OptionWriter[T: Writer]: Writer[Option[T]] = new Writer[Option[T]] {
    override def write[R](out: Facade[R], v: Option[T]): R = v match{
      case None    => out.jnull(-1)
      case Some(s) => implicitly[Writer[T]].write(out, s)
    }
  }

  override implicit def OptionReader[T: Reader]: Reader[Option[T]] = new Reader[Option[T]] {
    val delegate = implicitly[Reader[T]]
    override def jnull(index: Int) = None
    override def jfalse(index: Int) = Some(delegate.jfalse(index))
    override def jtrue(index: Int) = Some(delegate.jtrue(index))
    override def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      Some(delegate.jnum(s, decIndex, expIndex, index))
    }
    override def jstring(s: CharSequence, index: Int) = Some(delegate.jstring(s, index))

    override def objectContext(index: Int) = {
      upickle.Util.mapContext(delegate.objectContext(index))(Some(_))
    }
    override def arrayContext(index: Int) = {
      upickle.Util.mapContext(delegate.arrayContext(index))(Some(_))
    }
  }
}
// end_ex

object OptionsAsNullTests extends TestSuite {

  import OptionPickler._
  implicit def rw: OptionPickler.ReadWriter[Thing] = OptionPickler.macroRW
  val tests = TestSuite {
    'nullAsNone {

      // Quick check to ensure we didn't break anything
      'primitive {
        write("A String") ==> "\"A String\""
        read[String]("\"A String\"") ==> "A String"
        write(1) ==> "1"
        read[Int]("1") ==> 1
        write(Thing(1, "gg")) ==> """{"myFieldA": 1, "myFieldB": "gg"}"""
        read[Thing]("""{"myFieldA": 1, "myFieldB": "gg"}""") ==> Thing(1, "gg")
      }

      'none {
        write[None.type](None) ==> "null"
        read[None.type]("null") ==> None
      }

      'some {
        write(Some("abc")) ==> "\"abc\""
        read[Some[String]]("\"abc\"") ==> Some("abc")
        write(Some(1)) ==> "1"
        read[Some[Int]]("1") ==> Some(1)
        write(Some(3.14159)) ==> "3.14159"
        read[Some[Double]]("3.14159") ==> Some(3.14159)
      }

      'option {
        write(Option("abc")) ==> "\"abc\""
        read[Option[String]]("\"abc\"") ==> Some("abc")
      }

      'caseClass {
        write(Opt(None, None)) ==> """{"a": null, "b": null}"""
        read[Opt]("""{"a": null, "b": null}""") ==> Opt(None, None)
        write(Opt(Some("abc"), Some(1))) ==> """{"a": "abc", "b": 1}"""
      }

      'optionCaseClass {
        implicit val thingReader = implicitly[Reader[Thing]]
        implicit val thingWriter = implicitly[Writer[Thing]]

        write(Opt(None, None)) ==> """{"a": null, "b": null}"""
        read[Opt]("""{"a": null, "b": null}""") ==> Opt(None, None)
        write(Opt(Some("abc"), Some(1))) ==> """{"a": "abc", "b": 1}"""

        write(Option(Thing(1, "gg"))) ==> """{"myFieldA": 1, "myFieldB": "gg"}"""
        read[Option[Thing]]("""{"myFieldA": 1, "myFieldB": "gg"}""") ==> Option(Thing(1, "gg"))
      }

    }
  }
}