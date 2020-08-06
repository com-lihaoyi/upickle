package upickle

import scala.language.implicitConversions
import utest._

import upickle.default.{ read, write, Reader, ReadWriter }

sealed trait Animal
object Animal {
  given ReadWriter[Animal] = upickle.default.macroRW
}
case class Person(name: String, address: String,
  age: Int = 20) extends Animal
object Person {
  given ReadWriter[Person] = upickle.default.macroRW
}

case class Cat(name: String, owner: Person)
  extends Animal
object Cat {
  given ReadWriter[Cat] = upickle.default.macroRW
}

case class Dog(name: String, age: Int)
object Dog {
  given ReadWriter[Dog] = upickle.default.macroRW
}

object Singleton

object DerivationTests extends TestSuite {

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
      println(result == expected)
    }

    test("traitWriter") - {
      val person: Animal = Person("Peter", "Avenue 10 Zurich")
      val result = write(person)
      val expected = """{"$type":"upickle.Person","name":"Peter","address":"Avenue 10 Zurich"}"""
      assert(result == expected)
    }

    test("readWritrer") - {
      val rw = summon[ReadWriter[Animal]]
      val person = Person("Peter", "Somewhere", 30)
      val json = write(person)(rw)
      val expectedJson = """{"$type":"upickle.Person","name":"Peter","address":"Somewhere","age":30}"""
      assert(json == expectedJson)
      val deserialized = read(json)(rw)
      val expectedDeserialized = Person("Peter", "Somewhere", 30)
      assert(deserialized == expectedDeserialized)
    }
  }
}
