package upack

import utest._

object BoolTests extends TestSuite {
  def tests = Tests{
    test("upack.Bool apply") {
      upack.Bool(true) ==> upack.True
      upack.Bool(false) ==> upack.False
    }

    test("upack.Bool.value") {
      upack.Bool(true).value ==> true
      upack.Bool(false).value ==> false
    }
    test("upack.Bool unapply") {
      for(bool <- Seq(true, false)){
        val jsb = upack.Bool(bool)
        assertMatch(jsb) {
          case upack.Bool(value)
            if value == bool
            && jsb.value == value =>
        }
      }
    }
    test("upack.Bool pattern matching should be exhaustive") {
      upack.Bool(true) match {
        case upack.Bool(value) =>
      }
    }
  }
}
