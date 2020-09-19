package ujson

import org.scalatest.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class FileJNumIndexCheck extends AnyPropSpec with Matchers with ScalaCheckPropertyChecks {

  property("visitFloat64StringParts provides the correct indices with parseFromFile") {
    forAll { (value: BigDecimal) =>
      val json = s"""{ "num": ${value.toString} }"""
      TestUtil.withTemp(json) { t =>
        Readable.fromFile(t).transform(JNumIndexCheckFacade) shouldBe true
      }
    }
  }

  property("visitFloat64StringParts provides the correct indices at the top level with parseFromFile") {
    forAll { (value: BigDecimal) =>
      TestUtil.withTemp(value.toString) { t =>
        Readable.fromFile(t).transform(JNumIndexCheckFacade) shouldBe true
      }
    }
  }
}
