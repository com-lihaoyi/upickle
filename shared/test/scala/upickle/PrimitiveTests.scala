package upickle
import utest._
import scala.collection.SortedSet
import scala.concurrent.duration._
import TestUtil._

object PrimitiveTests extends TestSuite{

  def tests = TestSuite{
    'Boolean{
      'true-rw(true, "true")
      'false-rw(false, "false")
    }
    'String{
      'plain-rw("i am a cow", """ "i am a cow" """)
      'quotes-rw("i am a \"cow\"", """ "i am a \"cow\"" """)
      'unicode-rw("叉烧包")
    }

    'Int{
      'small-rw(1, "1")
      'med-rw(125123, "125123")
      'min-rw(Int.MinValue, "-2147483648")
      'max-rw(Int.MaxValue, "2147483647")
    }  

    'Double{
      'whole-rw(125123: Double, """125123.0""", """125123""")
      'fractional-rw(125123.1542312, """125123.1542312""")
      'negative-rw(-125123.1542312, """-125123.1542312""")
    }

    'Short{
      'simple-rw(25123: Short, "25123")
      'min-rw(Short.MinValue, "-32768")
      'max-rw(Short.MaxValue, "32767")
//      'all{
//        for (i <- Short.MinValue to Short.MaxValue by 100) rw(i)
//      }
    }

    'Byte{
      'simple-rw(125: Byte, "125")
      'min-rw(Byte.MinValue, "-128")
      'max-rw(Byte.MaxValue, "127")
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
