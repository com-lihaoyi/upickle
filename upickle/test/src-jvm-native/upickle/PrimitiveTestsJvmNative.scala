package upickle
import utest._
import upickle.legacy.read
import TestUtil._

// ScalaJS has JVM compatible Float semantics only with
// scalaJSLinkerConfig ~= { _.withSemantics(_.withStrictFloats(true)) }
// which is not currently supported by Mill
object PrimitiveTestsJvmNative extends TestSuite {

  def tests = Tests {
    test("Int"){
      test("max") - rwNum(Int.MaxValue, "2147483647")
    }

    test("Float"){
      test("max") - rwNum(Float.MaxValue, "3.4028235E38")
      test("inf") - rwNum(Float.PositiveInfinity, """ "Infinity" """)
    }

    test("maps"){
      test("float") - rw(
        Map(1F -> 2F, 3.3F -> 4.4F, 5.55F -> 6.66F, Float.MaxValue -> Float.MinValue),
        """{"1": 2, "3.3": 4.4, "5.55": 6.66, "3.4028235E38": -3.4028235E38}""",
        """[[1, 2], [3.3, 4.4], [5.55, 6.66], [3.4028235E38, -3.4028235E38]]""",
        """[["1", 2], ["3.3", 4.4], ["5.55", 6.66], ["3.4028235E38", -3.4028235E38]]"""
      )
      test("double") - rw(
        Map(1D -> 2D, 3.3D -> 4.4D, 5.55D -> 6.66D, Double.MaxValue -> Double.MinValue),
        """{"1": 2, "3.3": 4.4, "5.55": 6.66, "1.7976931348623157E308": -1.7976931348623157E308}""",
        """[[1, 2], [3.3, 4.4], [5.55, 6.66], [1.7976931348623157E308, -1.7976931348623157E308]]""",
        """[["1", 2], ["3.3", 4.4], ["5.55", 6.66], ["1.7976931348623157E308", -1.7976931348623157E308]]"""
      )
    }
  }
}
