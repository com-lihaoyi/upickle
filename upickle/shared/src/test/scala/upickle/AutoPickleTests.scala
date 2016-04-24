package upickle
import utest._
import acyclic.file

object AutopickleCases {
  sealed trait Foo
  case class A(a: Int, b: String) extends Foo
  case object B extends Foo

  object Foo {
    def pklR: upickle.default.Reader[Foo] = upickle.default.macroR[Foo]
    implicit val pklW: upickle.default.Writer[Foo] = upickle.default.macroW[Foo]
  }

  case class Bar(b: String, a: Double)
  object Bar {
    implicit val pkl = {
      upickle.default.macroRW[Bar]
    }
  }
}
object AutopickleTests extends TestSuite {
  import AutopickleCases._
  val tests = TestSuite {
    "autoWrite" - {
      assert(implicitly[upickle.default.Writer[Foo]] eq Foo.pklW)
    }
    "autoRead" - {
      val reader = implicitly[upickle.default.Reader[Foo]]

      assert(!(reader eq Foo.pklR))
    }
    "autoRW" - {
      val res = upickle.default.read[Bar](upickle.default.write(Bar("aaa",42.0)))
      assert(res == Bar("aaa",42.0))
    }
  }
}