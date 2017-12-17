package upickle
import utest._
import upickle.legacy.read
import TestUtil._

object PrimitiveTests extends TestSuite {

  def tests = Tests {
    'Unit{
      rw((), "{}")
    }
    'Boolean{
      'true-rw(true, "true")
      'false-rw(false, "false")
      'null-assert(read[Boolean]("null") == false)
    }
    'String{
      'plain-rw("i am a cow", """ "i am a cow" """)
      'quotes-rw("i am a \"cow\"", """ "i am a \"cow\"" """)
      'unicode-rw("叉烧包")
      'null-rw(null: String, "null")
    }
    'Symbol{
      'plain-rw('i_am_a_cow, """ "i_am_a_cow" """)
      'unicode-rw('叉烧包, """ "叉烧包" """)
      'null-rw(null: Symbol, "null")
    }
    'Long{
      'small-rw(1: Long, """ "1" """)
      'med-rw(125123: Long, """ "125123" """)
      'min-rw(Int.MinValue.toLong - 1, """ "-2147483649" """)
      'max-rw(Int.MaxValue.toLong + 1, """ "2147483648" """)
      'min-rw(Long.MinValue, """ "-9223372036854775808" """)
      'max-rw(Long.MaxValue, """ "9223372036854775807" """)
      'null-assert(read[Long]("null") == 0)
    }
    'BigInt{
      'whole-rw(BigInt("125123"), """ "125123" """)
      'fractional-rw(BigInt("1251231542312"), """ "1251231542312" """)
      'negative-rw(BigInt("-1251231542312"), """ "-1251231542312" """)
      'big-rw(
        BigInt("23420744098430230498023841234712512315423127402740234"),
          """ "23420744098430230498023841234712512315423127402740234" """)
      'null-rw(null: BigInt, "null")
    }
    'BigDecimal{
      'whole-rw(BigDecimal("125123"), """ "125123" """)
      'fractional-rw(BigDecimal("125123.1542312"), """ "125123.1542312" """)
      'negative-rw(BigDecimal("-125123.1542312"), """ "-125123.1542312" """)
      'big-rw(
        BigDecimal("234207440984302304980238412.15423127402740234"),
          """ "234207440984302304980238412.15423127402740234" """)
      'null-rw(null: BigDecimal, "null")
    }

    'Int{
      'small-rw(1, "1")
      'med-rw(125123, "125123")
      'min-rw(Int.MinValue, "-2147483648")
      'max-rw(Int.MaxValue, "2147483647")
      'null-assert(read[Int]("null") == 0)
    }

    'Double{
      'whole-rw(125123: Double, """125123.0""", """125123""")
      'wholeLarge-rw(1475741505173L: Double, """1475741505173.0""", """1475741505173""")
      'fractional-rw(125123.1542312, """125123.1542312""")
      'negative-rw(-125123.1542312, """-125123.1542312""")
      'null-assert(read[Double]("null") == 0.0)
      'nan-assert(
        java.lang.Double.isNaN(read[Double](""" "NaN" """)),
        upickle.default.write(Double.NaN) == "\"NaN\""
      )




    }

    'Short{
      'simple-rw(25123: Short, "25123")
      'min-rw(Short.MinValue, "-32768")
      'max-rw(Short.MaxValue, "32767")
      'null-assert(read[Short]("null") == 0)
//      'all{
//        for (i <- Short.MinValue to Short.MaxValue by 100) rw(i)
//      }
    }

    'Byte{
      'simple-rw(125: Byte, "125")
      'min-rw(Byte.MinValue, "-128")
      'max-rw(Byte.MaxValue, "127")
      'null-assert(read[Byte]("null") == 0)
      'all{
        for (i <- Byte.MinValue to Byte.MaxValue by 10) rw(i)
      }
    }

    'Float{
      'simple-rw(125.125f, """125.125""")
      'max-rw(Float.MaxValue)
      'min-rw(Float.MinValue)
      'minPos-rw(Float.MinPositiveValue)
      'inf-rw(Float.PositiveInfinity, """ "Infinity" """)
      "neg-inf" - rw(Float.NegativeInfinity, """ "-Infinity" """)
      'null-assert(read[Float]("null") == 0.0)
      'nan-assert(
        java.lang.Float.isNaN(read[Float](""" "NaN" """)),
        upickle.default.write(Float.NaN) == "\"NaN\""
      )
    }

    'Char{
      'f-rw('f', """ "f" """)
      'plus-rw('+', """ "+" """)

      'all{
        for(i <- Char.MinValue until Char.MaxValue by 100) {
          rw(i)
        }
      }
    }
  }
}
