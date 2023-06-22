package upickle.core

import utest._

object LinkedHashMapTests extends TestSuite {
  def tests = Tests {
    test("should keep insertion order") {
      val map = LinkedHashMap[String, String]()
      map("1") = "0"
      map("0") = "0"
      map.toList ==>
        "1" -> "0" ::
        "0" -> "0" :: Nil
    }
    test("should not allow null as key") {
      val map = LinkedHashMap[String, String]()
      intercept[NullPointerException] { map(null.asInstanceOf[String]) = "foo" }.getMessage ==>
        "null keys are not allowed"
    }
  }
}
