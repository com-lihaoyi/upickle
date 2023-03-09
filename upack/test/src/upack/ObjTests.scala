package upack

import utest._
import scala.collection.mutable

object ObjTests extends TestSuite {
  def tests = Tests {
    test("should keep insertion order") {
      val obj = Obj(Str("1") -> Int32(0))
      obj.value += Str("-1") -> Int32(0)

      val Seq((first, _), (second, _)) = obj.value.toSeq

      first ==> Str("1")
      second ==> Str("-1")
    }
    test("should keep insertion order starting from empty Obj") {
      val obj = Obj()
      obj.value += Str("1") -> Int32(0)
      obj.value += Str("-1") -> Int32(0)

      val Seq((first, _), (second, _)) = obj.value.toSeq

      first ==> Str("1")
      second ==> Str("-1")
    }
    test("should keep insertion order when wrapping mutable.Map") {
      val obj = Obj(mutable.Map[Msg, Msg](Str("1") -> Int32(0)))
      obj.value += Str("-1") -> Int32(0)

      val Seq((first, _), (second, _)) = obj.value.toSeq

      first ==> Str("1")
      second ==> Str("-1")
    }
    test("toString") {
      Obj(Str("0") -> Int32(0)).toString ==> """Obj(Map(Str(0) -> Int32(0)))"""
    }
    test("equals uses deep equality") {
      val first = Obj(Str("0") -> Int32(0))
      val second = Obj(Str("0") -> Int32(0))
      first ==> second
    }
    test("hashCode uses deep equality") {
      val first = Obj(Str("0") -> Int32(0))
      val second = Obj(Str("0") -> Int32(0))
      first.hashCode ==> second.hashCode
    }
    test("should not allow a null key") {
      val ex = intercept[NullPointerException] {
        Obj(null.asInstanceOf[Msg] -> Int32(0))
      }
      ex.getMessage ==> "null keys are not allowed"
    }
  }
}
