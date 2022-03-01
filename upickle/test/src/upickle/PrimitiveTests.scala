package upickle
import utest._
import upickle.legacy.read
import TestUtil._

object PrimitiveTests extends TestSuite {

  def tests = Tests {
    test("Unit"){
      rw((), "null", "{}")
    }
    test("Boolean"){
      test("true") - rw(true, "true", "\"true\"")
      test("false") - rw(false, "false", "\"false\"")
      test("null") - assert(read[Boolean]("null") == false)
    }
    test("String"){
      test("plain") - rw("i am a cow", """ "i am a cow" """)
      test("quotes") - rw("i am a \"cow\"", """ "i am a \"cow\"" """)
      test("unicode"){
        test - rw("叉烧包", "\"叉烧包\"")
        test {upickle.default.write("叉烧包") ==> "\"叉烧包\""}
        test {upickle.default.write("叉烧包", escapeUnicode = true) ==> "\"\\u53c9\\u70e7\\u5305\""}
        test {upickle.default.read[String]("\"\\u53c9\\u70e7\\u5305\"") ==> "叉烧包"}
        test {upickle.default.read[String]("\"叉烧包\"") ==> "叉烧包"}
      }
      test("null") - rw(null: String, "null")
      test("chars"){
        for(i <- Char.MinValue until 55296/*Char.MaxValue*/) {
          rw(i.toString)
        }
      }
    }
    test("Long"){
      test("small") - rwNum(1: Long, "1", """ "1" """)
      test("med") - rwNum(125123: Long, "125123", """ "125123" """)
      test("minInt") - rwNum(Int.MinValue.toLong - 1, "-2147483649", """ "-2147483649" """)
      test("maxInt") - rwNum(Int.MaxValue.toLong + 1, "2147483648", """ "2147483648" """)
      test("min") - rwNum(Long.MinValue, """ "-9223372036854775808" """, "-9223372036854775808")
      test("max") - rwNum(Long.MaxValue, """ "9223372036854775807" """, "9223372036854775807")
      test("null") - assert(read[Long]("null") == 0)
      test("invalid") - intercept[NumberFormatException](upickle.default.transform("a").to[Long])
    }
    test("BigInt"){
      test("whole") - rw(BigInt("125123"), """ "125123" """)
      test("fractional") - rw(BigInt("1251231542312"), """ "1251231542312" """)
      test("negative") - rw(BigInt("-1251231542312"), """ "-1251231542312" """)
      test("big") - rw(
        BigInt("23420744098430230498023841234712512315423127402740234"),
          """ "23420744098430230498023841234712512315423127402740234" """)
      test("null") - rw(null: BigInt, "null")
    }
    test("BigDecimal"){
      test("whole") - rw(BigDecimal("125123"), """ "125123" """)
      test("fractional") - rw(BigDecimal("125123.1542312"), """ "125123.1542312" """)
      test("negative") - rw(BigDecimal("-125123.1542312"), """ "-125123.1542312" """)
      test("big") - rw(
        BigDecimal("234207440984302304980238412.15423127402740234"),
          """ "234207440984302304980238412.15423127402740234" """)
      test("null") - rw(null: BigDecimal, "null")
    }

    test("Int"){
      test("small") - rwNum(1, "1", """ "1" """)
      test("med") - rwNum(125123, "125123", """ "125123" """)
      test("min") - rwNum(Int.MinValue, "-2147483648", """ "-2147483648" """)
      test("null") - assert(read[Int]("null") == 0)
    }

    test("Double"){
      test("whole") - rwNum(125123: Double, """125123""", """125123.0""")
      test("wholeLarge") - rwNum(1475741505173L: Double, """1475741505173""", """1475741505173.0""")
      test("fractional") - rwNum(125123.1542312, """125123.1542312""", """ "125123.1542312" """)
      test("negative") - rwNum(-125123.1542312, """-125123.1542312""", """ "-125123.1542312" """)
      test("null") - assert(read[Double]("null") == 0.0)
      test("nan") - assert(
        java.lang.Double.isNaN(read[Double](""" "NaN" """)),
        upickle.default.write(Double.NaN) == "\"NaN\""
      )
    }

    test("Short"){
      test("simple") - rwNum(25123: Short, "25123", """ "25123" """)
      test("min") - rwNum(Short.MinValue, "-32768", """ "-32768" """)
      test("max") - rwNum(Short.MaxValue, "32767", """ "32767" """)
      test("null") - assert(read[Short]("null") == 0)
      test("all"){
        for (i <- Short.MinValue to Short.MaxValue) rwNum(i)
      }
    }

    test("Byte"){
      test("simple") - rwNum(125: Byte, "125", """ "125" """)
      test("min") - rwNum(Byte.MinValue, "-128", """ "-128" """)
      test("max") - rwNum(Byte.MaxValue, "127", """ "127" """)
      test("null") - assert(read[Byte]("null") == 0)
      test("all"){
        for (i <- Byte.MinValue to Byte.MaxValue) rwNum(i)
      }
    }

    test("Float"){
      test("simple") - rwNum(125.125f, """125.125""", """ "125.125" """)
      test("min") - rwNum(Float.MinValue)
      test("minPos") - rwNum(Float.MinPositiveValue)
      test("max") - rwNum(Float.MaxValue)
      test("neg-inf") - rwNum(Float.NegativeInfinity, """ "-Infinity" """)
      test("null") - assert(read[Float]("null") == 0.0)
      test("nan") - assert(
        java.lang.Float.isNaN(read[Float](""" "NaN" """)),
        upickle.default.write(Float.NaN) == "\"NaN\""
      )
    }

    test("Char"){
      test("f") - rwNoBinaryJson('f', """ "f" """)
      test("plus") - rwNoBinaryJson('+', """ "+" """)

      test("all"){
        for(i <- Char.MinValue until 55296/*Char.MaxValue*/) {
          rwNoBinaryJson(i)
          num(i)
        }
      }
    }
  }
}
