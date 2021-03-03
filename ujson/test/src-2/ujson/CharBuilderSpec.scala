package ujson

import org.scalatest._
import org.scalatest.propspec._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class CharBuilderSpec extends AnyPropSpec with Matchers with ScalaCheckPropertyChecks {

  property("append") {
    forAll { (xs: List[Char]) =>
      val builder = new ujson.util.CharBuilder
      xs.foreach(builder.append)
      builder.makeString shouldBe xs.mkString
    }
  }
}
