package ujson

import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck._

class BoolSpec extends propspec.AnyPropSpec with Matchers with ScalaCheckPropertyChecks with Inside {

  property("ujson.Bool apply") {
    ujson.Bool(true) shouldBe ujson.True
    ujson.Bool(false) shouldBe ujson.False
  }

  property("ujson.Bool.value") {
    forAll { bool: Boolean =>
      ujson.Bool(bool).value shouldBe bool
    }
  }

  property("ujson.Bool unapply") {
    forAll { bool: Boolean =>
      val jsb = ujson.Bool(bool)
      inside(jsb) {
        case ujson.Bool(value) =>
          value shouldBe bool
          jsb.value shouldBe value
      }
    }
  }
}
