package upickle
package json

import org.scalatest._
import org.scalatest.prop._

class CharBuilderSpec extends PropSpec with Matchers with PropertyChecks {

  property("append") {
    forAll { xs: List[Char] =>
      val builder = new upickle.json.util.CharBuilder
      xs.foreach(builder.append)
      builder.makeString shouldBe xs.mkString
    }
  }

  property("extend") {
    forAll { xs: List[String] =>
      val builder = new upickle.json.util.CharBuilder
      xs.foreach(builder.extend)
      builder.makeString shouldBe xs.mkString
    }
  }
}
