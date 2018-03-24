package ujson

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class FileJNumIndexCheck extends PropSpec with Matchers with PropertyChecks {

  property("visitNum provides the correct indices with parseFromFile") {
    forAll { (value: BigDecimal) =>
      val json = s"""{ "num": ${value.toString} }"""
      TestUtil.withTemp(json) { t =>
        Transformable.fromFile(t).transform(JNumIndexCheckFacade) shouldBe true
      }
    }
  }

  property("visitNum provides the correct indices at the top level with parseFromFile") {
    forAll { (value: BigDecimal) =>
      TestUtil.withTemp(value.toString) { t =>
        Transformable.fromFile(t).transform(JNumIndexCheckFacade) shouldBe true
      }
    }
  }
}
