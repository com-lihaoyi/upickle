package upickle
import utest._
import acyclic.file

object shared {
  object that {
    import common.Message
    case class That(common: Message)
    object That{
      implicit def rw: upickle.default.ReadWriter[That] = upickle.default.macroRW
    }
  }
  object other {
    import common.Message
    case class Other(common: Message)
    object Other{
      implicit def rw: upickle.default.ReadWriter[Other] = upickle.default.macroRW
    }
  }
  object common {
    case class Message(content: String)
    object Message{
      implicit def rw: upickle.default.ReadWriter[Message] = upickle.default.macroRW
    }
  }
}

object All {
  import shared.other._
  sealed trait Outers
  object Outers{
    implicit def rw: upickle.default.ReadWriter[Outers] = upickle.default.ReadWriter.merge(
      Out1.rw
    )
  }
  case class Out1(a: Other) extends Outers
  object Out1{
    implicit def rw: upickle.default.ReadWriter[Out1] = upickle.default.macroRW
  }

  import shared.that._
  import shared.common._
  sealed trait Inners extends Outers
  object Inners{
    implicit def rw: upickle.default.ReadWriter[Inners] = upickle.default.ReadWriter.merge(
      Inner1.rw,
      Inner2.rw
    )
  }
  case class Inner1(b: That) extends Inners
  object Inner1{
    implicit def rw: upickle.default.ReadWriter[Inner1] = upickle.default.macroRW
  }
  case class Inner2(a: Message) extends Inners
  object Inner2{
    implicit def rw: upickle.default.ReadWriter[Inner2] = upickle.default.macroRW
  }
}

object AdvancedTests extends TestSuite {
  import All._
  val tests = Tests {
    "complexTraits" - {
      val reader = implicitly[upickle.default.Reader[Outers]]
      val writer = implicitly[upickle.default.Writer[Outers]]
      assert(reader != null)
      assert(writer != null)
    }
  }
}
