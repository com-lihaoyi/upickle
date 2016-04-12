package upickle
import utest._
import acyclic.file

sealed trait Foo
case class A(a: Int, b: String) extends Foo
case object B extends Foo

object Foo {
  val pklR: upickle.default.Reader[Foo] = upickle.default.macroR[Foo]
  implicit val pklW: upickle.default.Writer[Foo] = upickle.default.macroW[Foo]
}

object AutopickleTests extends TestSuite {
  val tests = TestSuite {
    "autoWrite" - {
      assert(implicitly[upickle.default.Writer[Foo]] == Foo.pklW)
    }
    "autoRead" - {
      val reader = implicitly[upickle.default.Reader[Foo]]
      assert(reader != Foo.pklR)
    }
  }
}
