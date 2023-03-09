package ujson

import utest._
import scala.collection.mutable

object ObjTests extends TestSuite {
  def tests = Tests {
    test("should keep insertion order") {
      val obj = Obj("1" -> 0)
      obj("0") = 0

      obj.toString ==> """{"1":0,"0":0}"""
    }
    test("should keep insertion order starting from empty Obj") {
      val obj = Obj()
      obj("1") = 0
      obj("0") = 0

      obj.toString ==> """{"1":0,"0":0}"""
    }
    test("should keep insertion order when wrapping mutable.Map") {
      val obj = Obj(mutable.Map[String, Value]("1" -> 0))
      obj("0") = 0
      val Seq(first, second) = obj.value.keys.toSeq

      obj.toString ==> """{"1":0,"0":0}"""
    }
    test("should keep insertion order when reading json") {
      val json = """{"0":0,"1":0}"""
      val written = write(read(json))

      written ==> json
    }
    test("toString renders to json") {
      Obj("0" -> 0).toString ==> """{"0":0}"""
    }
    test("equals uses deep equality") {
      val first = Obj("0" -> 0)
      val second = Obj("0" -> 0)
      first ==> second
    }
    test("hashCode uses deep equality") {
      val first = Obj("0" -> 0)
      val second = Obj("0" -> 0)
      first.hashCode ==> second.hashCode
    }
    test("should not allow a null key") {
      val ex = intercept[NullPointerException] {
        Obj(null.asInstanceOf[String] -> 0)
      }
      ex.getMessage ==> "null keys are not allowed"
    }
  }
}