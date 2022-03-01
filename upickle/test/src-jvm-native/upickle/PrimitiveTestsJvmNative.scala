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
      test("max") - rwNum(Float.MaxValue, "3.4028234663852886E38")
      test("inf") - rwNum(Float.PositiveInfinity, """ "Infinity" """)
    }
  }
}
