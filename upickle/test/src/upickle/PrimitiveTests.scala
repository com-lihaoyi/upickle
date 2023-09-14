package upickle
import utest._
import upickle.default.{read, write, readBinary, writeBinary, writeMsg, transform}
import TestUtil._

object PrimitiveTests extends TestSuite {

  def tests = Tests {
    test("Unit"){
      rw((), "null", "{}", upack.Null, upack.Obj())
    }
    test("Boolean"){
      test("true") - rw(true, "true", "\"true\"", upack.True, upack.Str("true"))
      test("false") - rw(false, "false", "\"false\"", upack.False, upack.Str("false"))
      test("null") - {
        assert(read[Boolean]("null") == false)
        assert(readBinary[Boolean](upack.Null) == false)
      }
    }
    test("String"){
      test("plain") - rw("i am a cow", """ "i am a cow" """, upack.Str("i am a cow"))
      test("quotes") - rw("i am a \"cow\"", """ "i am a \"cow\"" """, upack.Str("i am a \"cow\""))
      test("unicode"){
        test - rw("叉烧包", "\"叉烧包\"", upack.Str("叉烧包"))
        test - rwEscape("叉烧包", "\"\\u53c9\\u70e7\\u5305\"")
      }
      test("null") - rw(null: String, "null", upack.Null)
      test("chars"){
        for(i <- Char.MinValue until 55296/*Char.MaxValue*/) {
          rw(i.toString)
        }
      }
      test("others"){
        assert(upickle.default.read[String]("1") == "1")
        assert(upickle.default.read[String]("true") == "true")
        assert(upickle.default.read[Seq[String]]("[1, 2, 3]") == Seq("1", "2", "3"))
        assert(
          upickle.default.read[Map[String, String]]("""{"a":11111111111111111111111111111111}""") ==
          Map("a" -> "11111111111111111111111111111111")
        )
      }
    }
    test("Long"){
      test("small") - rwNum(1: Long, "1", """ "1" """, upack.Int64(1), upack.Str("1"))
      test("med") - rwNum(
        125123: Long,
        "125123", """ "125123" """,
        upack.Int64(125123), upack.Str("125123")
      )
      test("minInt") - rwNum(
        Int.MinValue.toLong - 1,
        "-2147483649", """ "-2147483649" """,
        upack.Int64(-2147483649L), upack.Str("-2147483649")
      )
      test("maxInt") - rwNum(
        Int.MaxValue.toLong + 1,
        "2147483648", """ "2147483648" """,
        upack.Int64(2147483648L), upack.Str("2147483648")
      )
      test("min") - rwNum(
        Long.MinValue,
        """ "-9223372036854775808" """, "-9223372036854775808",
        upack.Int64(-9223372036854775808L), upack.Str("-9223372036854775808")
      )
      test("max") - rwNum(
        Long.MaxValue,
        """ "9223372036854775807" """, "9223372036854775807",
        upack.Int64(9223372036854775807L), upack.Str("9223372036854775807")
      )
      test("null") - {
        assert(read[Long]("null") == 0)
        assert(readBinary[Long](upack.Null) == 0)
      }
      test("invalid") - intercept[NumberFormatException](transform("a").to[Long])
    }
    test("BigInt"){
      test("whole") - rw(BigInt("125123"), """ "125123" """, upack.Str("125123"))
      test("fractional") - rw(
        BigInt("1251231542312"),
        """ "1251231542312" """, upack.Str("1251231542312")
      )
      test("negative") - rw(
        BigInt("-1251231542312"),
        """ "-1251231542312" """, upack.Str("-1251231542312")
      )
      test("big") - rw(
        BigInt("23420744098430230498023841234712512315423127402740234"),
          """ "23420744098430230498023841234712512315423127402740234" """,
        upack.Str("23420744098430230498023841234712512315423127402740234")
      )
      test("null") - rw(null: BigInt, "null", upack.Null)
    }
    test("BigDecimal"){
      test("whole") - rw(BigDecimal("125123"), """ "125123" """, upack.Str("125123"))
      test("fractional") - rw(
        BigDecimal("125123.1542312"),
        """ "125123.1542312" """, upack.Str("125123.1542312")
      )
      test("negative") - rw(
        BigDecimal("-125123.1542312"),
        """ "-125123.1542312" """, upack.Str("-125123.1542312")
      )
      test("big") - rw(
        BigDecimal("234207440984302304980238412.15423127402740234"),
        """ "234207440984302304980238412.15423127402740234" """,
        upack.Str("234207440984302304980238412.15423127402740234")
      )
      test("null") - rw(null: BigDecimal, "null", upack.Null)
    }

    test("Int"){
      test("small") - rwNum(1, "1", """ "1" """, upack.Int32(1), upack.Str("1"))
      test("med") - rwNum(
        125123,
        "125123", """ "125123" """,
        upack.Int32(125123), upack.Str("125123")
      )
      test("min") - rwNum(
        Int.MinValue,
        "-2147483648", """ "-2147483648" """,
        upack.Int32(-2147483648), upack.Str("-2147483648")
      )
      test("null") - {
        assert(read[Int]("null") == 0)
        assert(readBinary[Int](upack.Null) == 0)
      }
    }

    test("Double"){
      test("whole") - rwNum(
        125123: Double,
        """125123""", """125123.0""",
        upack.Float64(125123), upack.Str("125123.0")
      )
      test("wholeLarge") - rwNum(
        1475741505173L: Double,
        """1475741505173""", """1475741505173.0""",
        upack.Float64(1475741505173.0), upack.Str("1475741505173.0")
      )
      test("fractional") - rwNum(
        125123.1542312,
         """125123.1542312""", """ "125123.1542312" """,
        upack.Float64(125123.1542312), upack.Str("125123.1542312")
      )
      test("negative") - rwNum(
        -125123.1542312,
        """-125123.1542312""", """ "-125123.1542312" """,
        upack.Float64(-125123.1542312), upack.Str("-125123.1542312")
      )
      test("null") - {
        assert(read[Double]("null") == 0.0)
        assert(readBinary[Double](upack.Null) == 0.0)
      }
      test("nan") - {
        assert(
          java.lang.Double.isNaN(read[Double](""" "NaN" """)),
          write(Double.NaN) == "\"NaN\""
        )
        assert(java.lang.Double.isNaN(readBinary[Double](upack.Float64(Double.NaN))))
        val written = writeMsg(Double.NaN)
        assert(java.lang.Double.isNaN(written.asInstanceOf[upack.Float64].value))
      }
    }

    test("Short"){
      test("simple") - rwNum(
        25123: Short,
        "25123", """ "25123" """,
        upack.Int32(25123), upack.Str("25123")
      )
      test("min") - rwNum(
        Short.MinValue,
        "-32768", """ "-32768" """,
        upack.Int32(-32768), upack.Str("-32768")
      )
      test("max") - rwNum(
        Short.MaxValue,
        "32767", """ "32767" """,
        upack.Int32(32767), upack.Str("32767")
      )
      test("null") - {
        assert(read[Short]("null") == 0)
        assert(readBinary[Short](upack.Null) == 0)
      }
      test("all"){
        for (i <- Short.MinValue to Short.MaxValue) rwNum(i)
      }
    }

    test("Byte"){
      test("simple") - rwNum(
        125: Byte,
        "125", """ "125" """,
        upack.Int32(125), upack.Str("125")
      )
      test("min") - rwNum(
        Byte.MinValue,
        "-128", """ "-128" """,
        upack.Int32(-128), upack.Str("-128")
      )
      test("max") - rwNum(
        Byte.MaxValue,
        "127", """ "127" """,
        upack.Int32(127), upack.Str("127")
      )
      test("null") - {
        assert(read[Byte]("null") == 0)
        assert(readBinary[Byte](upack.Null) == 0)
      }
      test("all"){
        for (i <- Byte.MinValue to Byte.MaxValue) rwNum(i)
      }
    }

    test("Float"){
      test("simple") - rwNum(
        125.125f,
        """125.125""", """ "125.125" """,
        upack.Float32(125.125f), upack.Str("125.125")
      )
      test("min") - rwNum(Float.MinValue)
      test("minPos") - rwNum(Float.MinPositiveValue)
      test("neg-inf") - rwNum(
        Float.NegativeInfinity,
        """ "-Infinity" """,
        upack.Float32(Float.NegativeInfinity)
      )
      test("null") - {
        assert(read[Float]("null") == 0.0f)
        assert(readBinary[Float](upack.Null) == 0.0f)
      }
      test("nan") - {
        assert(
          java.lang.Float.isNaN(read[Float](""" "NaN" """)),
          write(Float.NaN) == "\"NaN\""
        )

        assert(java.lang.Float.isNaN(readBinary[Float](upack.Float32(Float.NaN))))
        val written = writeMsg(Float.NaN)
        assert(java.lang.Float.isNaN(written.asInstanceOf[upack.Float32].value))
      }
    }

    test("Char"){
      test("f") - rwNoBinaryJson('f', """ "f" """, upack.Int32('f'))
      test("plus") - rwNoBinaryJson('+', """ "+" """, upack.Int32('+'))

      test("all"){
        for(i <- Char.MinValue until 55296/*Char.MaxValue*/) {
          rwNoBinaryJson(i)
          num(i)
        }
      }
    }

    test("java"){
      test("bool"){
        test("value") - rw(true: java.lang.Boolean, "true", "\"true\"", upack.True, upack.Str("true"))
        test("null") - rw(null: java.lang.Boolean, "null", upack.Null)
      }
      test("byte"){
        test("value") - rw((1: Byte): java.lang.Byte, "1", """ "1" """, upack.Int32(1), upack.Str("1"))
        test("null") - rw(null: java.lang.Byte, "null", upack.Null)
      }
      test("char") {
        test("value") - rwNoBinaryJson('f': java.lang.Character, """ "f" """, upack.Int32('f'))
        test("null") - rw(null: java.lang.Character, "null", upack.Null)
      }
      test("short") {
        test("value") - rw(
          (25123: Short): java.lang.Short,
          "25123", """ "25123" """,
          upack.Int32(25123), upack.Str("25123")
        )
        test("null") - rw(null: java.lang.Short, "null", upack.Null)
      }
      test("int"){
        test("value") - rw(1: java.lang.Integer, "1", """ "1" """, upack.Int32(1), upack.Str("1"))
        test("null") - rw(null: java.lang.Integer, "null", upack.Null)
      }

      test("long") {
        test("value") - rw(1: java.lang.Long, "1", """ "1" """, upack.Int64(1), upack.Str("1"))
        test("null") - rw(null: java.lang.Long, "null", upack.Null)
      }

      test("float") {
        test("value") - rw(
          125.125f: java.lang.Float,
          """125.125""", """ "125.125" """,
          upack.Float32(125.125f), upack.Str("125.125")
        )
        test("null") - rw(null: java.lang.Float, "null", upack.Null)
      }
      test("double"){
        test("value") - rw(
          125123: java.lang.Double,
          """125123""", """125123.0""",
          upack.Float64(125123), upack.Str("125123.0")
        )
        test("null") - rw(null: java.lang.Double, "null", upack.Null)
      }
    }
  }
}
