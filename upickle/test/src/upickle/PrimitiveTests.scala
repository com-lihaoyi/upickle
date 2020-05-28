package upickle
import utest._
import upickle.legacy.read
import TestUtil._

object PrimitiveTests extends TestSuite {

  def tests = Tests {
    test("Unit"){
      rw((), "{}")
    }
    test("Boolean"){
      test("true") - rw(true, "true")
      test("false") - rw(false, "false")
      test("null") - assert(read[Boolean]("null") == false)
    }
    test("String"){
      test("plain") - rw("i am a cow", """ "i am a cow" """)
      test("quotes") - rw("i am a \"cow\"", """ "i am a \"cow\"" """)
      test("unicode"){
        rw("叉烧包")
        upickle.default.write("叉烧包") ==> "\"叉烧包\""
        upickle.default.write("叉烧包", escapeUnicode = true) ==> "\"\\u53c9\\u70e7\\u5305\""
        upickle.default.read[String]("\"\\u53c9\\u70e7\\u5305\"") ==> "叉烧包"
        upickle.default.read[String]("\"叉烧包\"") ==> "叉烧包"
      }
      test("null") - rw(null: String, "null")
      test("chars"){
        for(i <- Char.MinValue until 55296/*Char.MaxValue*/) {
          rw(i.toString)
        }
      }
    }
    test("Symbol"){
      test("plain") - rw("i_am_a_cow", """ "i_am_a_cow" """)(upickle.default.SymbolReader, upickle.default.SymbolWriter)
      test("unicode") - rw("叉烧包", """ "叉烧包" """)
      test("null") - rw(null: Symbol, "null")
    }
    test("Long"){
      test("small") - rw(1: Long, """ "1" """)
      test("med") - rw(125123: Long, """ "125123" """)
      test("min") - rw(Int.MinValue.toLong - 1, """ "-2147483649" """)
      test("max") - rw(Int.MaxValue.toLong + 1, """ "2147483648" """)
      test("min") - rw(Long.MinValue, """ "-9223372036854775808" """)
      test("max") - rw(Long.MaxValue, """ "9223372036854775807" """)
      test("null") - assert(read[Long]("null") == 0)
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
      test("small") - rw(1, "1")
      test("med") - rw(125123, "125123")
      test("min") - rw(Int.MinValue, "-2147483648")
      test("max") - rw(Int.MaxValue, "2147483647")
      test("null") - assert(read[Int]("null") == 0)
    }

    test("Double"){
      test("whole") - rw(125123: Double, """125123.0""", """125123""")
      test("wholeLarge") - rw(1475741505173L: Double, """1475741505173.0""", """1475741505173""")
      test("fractional") - rw(125123.1542312, """125123.1542312""")
      test("negative") - rw(-125123.1542312, """-125123.1542312""")
      test("null") - assert(read[Double]("null") == 0.0)
      test("nan") - assert(
        java.lang.Double.isNaN(read[Double](""" "NaN" """)),
        upickle.default.write(Double.NaN) == "\"NaN\""
      )




    }

    test("Short"){
      test("simple") - rw(25123: Short, "25123")
      test("min") - rw(Short.MinValue, "-32768")
      test("max") - rw(Short.MaxValue, "32767")
      test("null") - assert(read[Short]("null") == 0)
      test("all"){
        for (i <- Short.MinValue to Short.MaxValue) rw(i)
      }
    }

    test("Byte"){
      test("simple") - rw(125: Byte, "125")
      test("min") - rw(Byte.MinValue, "-128")
      test("max") - rw(Byte.MaxValue, "127")
      test("null") - assert(read[Byte]("null") == 0)
      test("all"){
        for (i <- Byte.MinValue to Byte.MaxValue) rw(i)
      }
    }

    test("Float"){
      test("simple") - rw(125.125f, """125.125""")
      test("max") - rw(Float.MaxValue)
      test("min") - rw(Float.MinValue)
      test("minPos") - rw(Float.MinPositiveValue)
      test("inf") - rw(Float.PositiveInfinity, """ "Infinity" """)
      "neg-inf" - rw(Float.NegativeInfinity, """ "-Infinity" """)
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
        for(i <- Char.MinValue until 55296/*Char.MaxValue*/) rwNoBinaryJson(i)
      }
    }
  }
}
