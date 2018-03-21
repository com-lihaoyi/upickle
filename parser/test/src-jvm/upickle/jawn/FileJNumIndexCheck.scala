package upickle
package jawn
package parser

import java.nio.ByteBuffer
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import scala.util.Success

class FileJNumIndexCheck extends PropSpec with Matchers with PropertyChecks {

  property("visitNum provides the correct indices with parseFromFile") {
    forAll { (value: BigDecimal) =>
      val json = s"""{ "num": ${value.toString} }"""
      TestUtil.withTemp(json) { t =>
        Parser.parseFromFile(t)(JNumIndexCheckFacade) shouldBe Success(true)
      }
    }
  }

  property("visitNum provides the correct indices at the top level with parseFromFile") {
    forAll { (value: BigDecimal) =>
      TestUtil.withTemp(value.toString) { t =>
        Parser.parseFromFile(t)(JNumIndexCheckFacade) shouldBe Success(true)
      }
    }
  }
}
