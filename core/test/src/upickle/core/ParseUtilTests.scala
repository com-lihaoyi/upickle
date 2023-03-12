package upickle.core

import utest._

import scala.util.Try

object ParseUtilTests extends TestSuite {

  val tests = Tests {

    test("parseLong - valid") {
      test("0")(ParseUtils.parseLong("0", 0, 1) ==> 0L)
      test("42")(ParseUtils.parseLong("42", 0, 2) ==> 42L)
      test("x0")(ParseUtils.parseLong("x0", 1, 2) ==> 0L)
      test("x0x")(ParseUtils.parseLong("x0x", 1, 2) ==> 0L)
      test("x-1x")(ParseUtils.parseLong("x-1x", 1, 3) ==> -1L)
    }

    test("parseLong - invalid") {
      def invalid(input: String)(start: Int = 0, end: Int = input.length) = {
        val e = intercept[NumberFormatException] {
          ParseUtils.parseLong(input, start, end)
        }
        e.getMessage ==> input.substring(start, end)
      }

      test("a")(invalid("a")())
      test("-")(invalid("-")())
      test("᥌")(invalid("᥌")())
      test("too long")(invalid(Long.MaxValue.toString + "1")())
      test("10x")(invalid("x10x")(1, 4))
      test("x10")(invalid("x10x")(0, 3))
    }

    test("parseLong - bounds") {
      val s = "123"
      for {
        start <- -1 to (s.length + 1)
          end <- -1 to (s.length + 1)
          if start != end // NumberFormatException, not IndexOutOfBoundsException
      } {
        // Roundabout way to avoid scala.js differences: https://github.com/scala-js/scala-js/issues/3546
        val isValidRange = 0 <= start && start <= end && end <= s.length
        if (isValidRange) {
          ParseUtils.parseLong(s, start, end)
        } else {
          intercept[java.lang.IndexOutOfBoundsException](ParseUtils.parseLong(s, start, end))
        }
      }
    }
  }
}
