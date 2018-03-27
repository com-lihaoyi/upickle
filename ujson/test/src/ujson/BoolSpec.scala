package ujson

import org.scalatest._
import org.scalatest.prop._

class BoolSpec extends PropSpec with Matchers with PropertyChecks with Inside {

  property("Js.Bool apply") {
    Js.Bool(true) shouldBe Js.True
    Js.Bool(false) shouldBe Js.False
  }

  property("Js.Bool.value") {
    forAll { bool: Boolean =>
      Js.Bool(bool).value shouldBe bool
    }
  }

  property("Js.Bool unapply") {
    forAll { bool: Boolean =>
      val jsb = Js.Bool(bool)
      inside(jsb) {
        case Js.Bool(value) =>
          value shouldBe bool
          jsb.value shouldBe value
      }
    }
  }
}
