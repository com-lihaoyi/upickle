//package upickle
//
//import scala.language.implicitConversions
//import TestUtil.rw
//import utest._
//
//import upickle.default.{ read, write, Reader, ReadWriter, macroRWAll }
////
////case class Dog(name: String, age: Int) derives ReadWriter
////
////sealed trait Animal derives ReadWriter
////case class Person(name: String, address: String, age: Int = 20) extends Animal
////case class Cat(name: String, owner: Person) extends Animal
////case object Cthulu extends Animal
////
////sealed trait Level1 derives ReadWriter
////case class Level1Cls(i: Int) extends Level1
////case object Level1Obj extends Level1
////sealed trait Level2 extends Level1 derives ReadWriter
////case class Level2Cls(s: String) extends Level2
////case object Level2Obj extends Level2
////sealed trait Level3 extends Level2 derives ReadWriter
////case class Level3Cls(b: Boolean) extends Level3
////case object Level3Obj extends Level3
//
//object DerivationTests extends TestSuite {
//  val tests = Tests {
//    test("example") {
//      test("dog"){
//        upickle.default.write(Dog("Ball", 2)) ==> """{"name":"Ball","age":2}"""
//        upickle.default.read[Dog]("""{"name":"Ball","age":2}""") ==> Dog("Ball", 2)
//      }
//      test("animal"){
//        upickle.default.write(Person("Peter", "Ave 10")) ==>
//          """{"$type":"upickle.Person","name":"Peter","address":"Ave 10"}"""
//
//        upickle.default.read[Animal]("""{"$type":"upickle.Person","name":"Peter","address":"Ave 10"}""") ==>
//          Person("Peter", "Ave 10")
//
//        upickle.default.write(Cthulu) ==> "\"upickle.Cthulu\""
//        upickle.default.read[Animal]("\"upickle.Cthulu\"") ==> Cthulu
//      }
//    }
//
//    test("caseClass") - {
//      rw[Dog](Dog("Ball", 2), """{"name":"Ball","age":2}""")
//    }
//
//    test("caseClassTagged") - {
//      rw[Person](
//        Person("Peter", "Avenue 10 Zurich", 20),
//        """{"$type":"upickle.Person","name":"Peter","address":"Avenue 10 Zurich"}"""
//      )
//    }
//
//    test("trait") - {
//      rw[Animal](
//        Person("Peter", "Avenue 10 Zurich" ,20),
//        """{"$type":"upickle.Person","name":"Peter","address":"Avenue 10 Zurich"}"""
//      )
//      rw[Animal](
//        Person("Peter", "Avenue 10 Zurich"),
//        """{"$type":"upickle.Person","name":"Peter","address":"Avenue 10 Zurich"}"""
//      )
//    }
//
//    test("caseObjectWriter") - {
//      rw[Animal](Cthulu, """"upickle.Cthulu"""", """{"$type":"upickle.Cthulu"}""")
//      rw[Cthulu.type](Cthulu, """"upickle.Cthulu"""", """{"$type":"upickle.Cthulu"}""")
//    }
//
//
//    test("recursive"){
//      case class Recur(recur: Option[Recur]) derives ReadWriter
//      rw(Recur(None), """{"recur":[]}""")
//      rw(Recur(Some(Recur(None))), """{"recur":[{"recur": []}]}""")
//    }
//    test("multilevel"){
//      rw(Level1Cls(1), """{"$type": "upickle.Level1Cls", "i": 1}""")
//      rw(Level1Cls(1): Level1, """{"$type": "upickle.Level1Cls", "i": 1}""")
//
//      rw(Level1Obj, """"upickle.Level1Obj"""")
//      rw(Level1Obj: Level1, """"upickle.Level1Obj"""")
//
//      rw(Level2Cls("str"), """{"$type": "upickle.Level2Cls", "s": "str"}""")
//      rw(Level2Cls("str"): Level2, """{"$type": "upickle.Level2Cls", "s": "str"}""")
//      rw(Level2Cls("str"): Level1, """{"$type": "upickle.Level2Cls", "s": "str"}""")
//
//      rw(Level2Obj, """"upickle.Level2Obj"""")
//      rw(Level2Obj: Level2, """"upickle.Level2Obj"""")
//      rw(Level2Obj: Level1, """"upickle.Level2Obj"""")
//
//      rw(Level3Cls(true), """{"$type": "upickle.Level3Cls", "b": true}""")
//      rw(Level3Cls(true): Level3, """{"$type": "upickle.Level3Cls", "b": true}""")
//      rw(Level3Cls(true): Level2, """{"$type": "upickle.Level3Cls", "b": true}""")
//      rw(Level3Cls(true): Level1, """{"$type": "upickle.Level3Cls", "b": true}""")
//
//      rw(Level3Obj, """"upickle.Level3Obj"""")
//      rw(Level3Obj: Level3, """"upickle.Level3Obj"""")
//      rw(Level3Obj: Level2, """"upickle.Level3Obj"""")
//      rw(Level3Obj: Level1, """"upickle.Level3Obj"""")
//    }
//
//
//
//    test("failures"){
//      test("caseClassTaggedWrong") - {
//        val e = intercept[upickle.core.AbortException] {
//          upickle.default.read[Person](
//            """{"$type":"upickle.Cat","name":"Peter","owner":{"$type":"upickle.Person","name": "bob", "address": "Avenue 10 Zurich"}}"""
//          )
//        }
//        assert(e.getMessage == "invalid tag for tagged object: upickle.Cat at index 9")
//      }
//
//      test("multilevelTaggedWrong") - {
//        val e = intercept[upickle.core.AbortException] {
//          upickle.default.read[Level2]("""{"$type": "upickle.Level1Cls", "i": 1}""")
//        }
//        assert(e.getMessage == "invalid tag for tagged object: upickle.Level1Cls at index 10")
//      }
//    }
//  }
//}
