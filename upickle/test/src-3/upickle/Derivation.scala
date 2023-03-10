package upickle

import scala.language.implicitConversions
import TestUtil.rw
import utest._

import upickle.default.{ read, write, Reader, ReadWriter }

sealed trait Animal derives ReadWriter

case class Person(name: String, address: String, age: Int = 20)
  extends Animal
  derives ReadWriter

case class Cat(name: String, owner: Person)
  extends Animal
  derives ReadWriter

case class Dog(name: String, age: Int)
  derives ReadWriter

case object Cthulu
  extends Animal
  derives ReadWriter

sealed trait AnimalImplicit

case object CthuluImplicit
  extends AnimalImplicit


object DerivationTests extends TestSuite {
//  implicit val rwCthuluImplicit: ReadWriter[CthuluImplicit.type] = ReadWriter.derived
//  implicit val rwAnimalImplicit: ReadWriter[AnimalImplicit] = ReadWriter.derived
  val tests = Tests {
    test("caseClass") - {
      rw[Dog](Dog("Ball", 2), """{"name":"Ball","age":2}""")
    }

    test("caseClassTagged") - {
      rw[Person](
        Person("Peter", "Avenue 10 Zurich", 20),
        """{"$type":"upickle.Person","name":"Peter","address":"Avenue 10 Zurich"}"""
      )
    }

    test("trait") - {
      rw[Animal](
        Person("Peter", "Avenue 10 Zurich" ,20),
        """{"$type":"upickle.Person","name":"Peter","address":"Avenue 10 Zurich"}"""
      )
      rw[Animal](
        Person("Peter", "Avenue 10 Zurich"),
        """{"$type":"upickle.Person","name":"Peter","address":"Avenue 10 Zurich"}"""
      )
    }

    test("caseObjectWriter") - {
      rw[Animal](Cthulu, """"upickle.Cthulu"""", """{"$type":"upickle.Cthulu"}""")
      rw[Cthulu.type](Cthulu, """"upickle.Cthulu"""", """{"$type":"upickle.Cthulu"}""")
    }

//    test("caseObjectWriterImplicit") - {
//      rw[AnimalImplicit](
//        CthuluImplicit,
//        """"upickle.CthuluImplicit"""",
//         """{"$type":"upickle.CthuluImplicit"}"""
//      )
//      rw[CthuluImplicit.type](
//        CthuluImplicit,
//         """"upickle.CthuluImplicit"""",
//         """{"$type":"upickle.CthuluImplicit"}"""
//      )
//    }

    test("recursive"){
      case class Recur(recur: Option[Recur]) derives ReadWriter
      rw(Recur(None), """{"recur":[]}""")
      rw(Recur(Some(Recur(None))), """{"recur":[{"recur": []}]}""")
    }
  }
}
