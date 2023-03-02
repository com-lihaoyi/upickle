package upickle

import scala.language.implicitConversions
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

//sealed trait Animal2
//case object Cthulu2
//  extends Animal2



object DerivationTests extends TestSuite {
//  implicit val rwAnimal2: ReadWriter[Animal2] = ReadWriter.derived
//  implicit val rwCthulu2: ReadWriter[Cthulu2.type] = ReadWriter.derived
  val tests = Tests {
    test("caseClassReader") - {
      val dogJson = """
        {
          "name": "Ball",
          "age": 2
        }
      """
      val parsed = read[Dog](dogJson)
      val expected = Dog("Ball", 2)
      assert(parsed == expected)
    }

    test("caseClassTaggedReader") - {
      val personJson = """
        {
          "$type":"upickle.Person",
          "name": "Peter",
          "address": "Avenue 10 Zurich"
        }
      """
      val parsed = read[Person](personJson)
      val expected = Person("Peter", "Avenue 10 Zurich", 20)
      assert(parsed == expected)
    }

    test("traitReader") - {
      val personJson = """
        {
          "$type":"upickle.Person",
          "name": "Peter",
          "address": "Avenue 10 Zurich"
        }
      """
      val parsed = read[Animal](personJson)
      val expected = Person("Peter", "Avenue 10 Zurich" ,20)
      assert(parsed == expected)
    }

    test("caseClassWriter") - {
      val dog = Dog("Ball", 10)
      val result = write(dog)
      val expected = """{"name":"Ball","age":10}"""
      assert(result == expected)
    }

    test("caseObjectWriter") - {
      val result1 = write(Cthulu)

      val animal: Animal = Cthulu
      val result2 = write(animal)

      val expected = """{"$type":"upickle.Cthulu"}"""
      assert(result1 == expected)
      assert(result2 == expected)
    }


//    test("caseObjectWriterImplicit") - {
//      val result1 = write(Cthulu2)
//
//      val animal: Animal2 = Cthulu2
//      val result2 = write(animal)
//
//      val expected = """{"$type":"upickle.Cthulu2"}"""
//      assert(result1 == expected)
//      assert(result2 == expected)
//    }

    test ("caseObjectReader") - {
      val json = """{"$type":"upickle.Cthulu"}"""
      val result = read[Animal](json)
      assert(result == Cthulu)
    }

    test("traitWriter") - {
      val person: Animal = Person("Peter", "Avenue 10 Zurich")
      val result = write(person)
      val expected = """{"$type":"upickle.Person","name":"Peter","address":"Avenue 10 Zurich"}"""
      assert(result == expected)
    }

    test("readWriter") - {
      val rw = summon[ReadWriter[Animal]]
      val person = Person("Peter", "Somewhere", 30)
      val json = write(person)(rw)
      val expectedJson = """{"$type":"upickle.Person","name":"Peter","address":"Somewhere","age":30}"""
      assert(json == expectedJson)
      val deserialized = read(json)(rw)
      val expectedDeserialized = Person("Peter", "Somewhere", 30)
      assert(deserialized == expectedDeserialized)
    }
    test("recursive"){
      case class Recur(recur: Option[Recur]) derives ReadWriter
      assert(write(Recur(None)) == """{"recur":[]}""")
    }
  }
}
