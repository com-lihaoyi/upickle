package upickle
import utest._
import acyclic.file

object shared {
  object that {
    import common.Message
    case class That(common: Message)
  }
  object other {
    import common.Message
    case class Other(common: Message)
  }
  object common {
    case class Message(content: String)
  }
}

object All {
  import shared.other._
  sealed trait Outers
  case class Out1(a: Other) extends Outers

  import shared.that._
  import shared.common._
  sealed trait Inners extends Outers
  case class Inner1(b: That) extends Inners
  case class Inner2(a: Message) extends Inners
}

object AdvancedTests extends TestSuite{
  import All._
  val tests = TestSuite{
    "complexTraits" - {
      val reader = implicitly[upickle.default.Reader[Outers]]
      val writer = implicitly[upickle.default.Writer[Outers]]
      assert(reader != null)
      assert(writer != null)
    }
  }
}
