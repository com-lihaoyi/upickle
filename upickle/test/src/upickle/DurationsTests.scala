package upickle

import utest._
import TestUtil._
import scala.concurrent.duration._

object DurationsTests extends TestSuite {
  val tests = Tests {
    test("durations"){
      test("inf") - rw(Duration.Inf, """ "inf" """)
      test("-inf") - rw(Duration.MinusInf, """ "-inf" """)
      test("undef") - rw(Duration.Undefined, """ "undef" """)
      test("1-second") - rw(1.second, """ "1000000000" """)
      test("2-hour") - rw(2.hours, """ "7200000000000" """)
    }
  }
}
