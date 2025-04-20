package upickle.implicits.namedTuples

import utest.*
import upickle.core.LinkedHashMap

object LensesTest extends TestSuite {

  val big_blob_json = ujson.write(
    ujson.Obj(
      LinkedHashMap(for
        a <- ('a' to 'z')
        n <- (0 to 10)
      yield s"$a$n" -> ujson.Obj(s"$a${n}_inner" -> ujson.Num(n)))
    )
  )

  val tests = Tests {
    test("ignore fields") {
      import upickle.implicits.namedTuples.default.given

      type Schema = (a10: (a10_inner: Int))

      val result: Schema =
        upickle.default.read[Schema](big_blob_json) // should not throw

      val expected: Schema = (a10 = (a10_inner = 10))
      assert(result == expected)
    }

    test("ignore fields with strict") {
      import upickle.implicits.namedTuples.default.strict.given

      type Schema = (a10: (a10_inner: Int))

      val e = intercept[upickle.core.AbortException] {
        // should throw, because it does not expect to see extra fields
        val result: Schema =
          upickle.default.read[Schema](big_blob_json)
      }
      assert(e.getMessage().contains("Unknown Key: a0"))
    }
  }

}
