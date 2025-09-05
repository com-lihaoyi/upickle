package upickletest

import ujson.ParseException
import upickletest.TestUtil.rw
import upickle.core.AbortException

import scala.language.implicitConversions
import utest.{assert, intercept, *}
import upickle.default.*

type AorB = "A" | "B"
type AorBorC = AorB | "C"

object UnionTests extends TestSuite {


  val tests = Tests {
    test("literal union"){
      test("strings"){
        rw[AorB]("A", "\"A\"")
        rw[AorBorC]("C", "\"C\"")
      }
    }
  }
}


