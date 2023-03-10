package ujson
import utest._
object BoolTests extends TestSuite {
  def tests = Tests{

    test("ujson.Bool apply") {
      ujson.Bool(true) ==> ujson.True
      ujson.Bool(false) ==> ujson.False
    }

    test("ujson.Bool.value") {

      ujson.Bool(true).value ==> true
      ujson.Bool(false).value ==> false
    }

    test("ujson.Bool unapply") {
      for(bool <- Seq(true, false)){
        val jsb = ujson.Bool(bool)
        assertMatch(jsb) {
          case ujson.Bool(value)
            if value == bool
            && jsb.value == value =>
        }
      }
    }
  }
}
