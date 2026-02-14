package upickle.jsonschema

import utest.*

object AdditionalSchemaCoverageSnapshotTests extends TestSuite {
  type NamedTupleExample = (foo: Seq[Int], bar: String, qux: Option[Int])
  type NamedTupleSchema = (
    (x: Int, y: Double, z: Long),
    (name: String, isHuman: Boolean, isAlien: Boolean),
    (arr: Seq[Int], optionalAny: Option[Int], optionalInt: Option[Int])
  )
  type NamedTupleMissingKeyShape = (foo: Boolean)

  val tests = Tests {
    test("Enum_SimpleEnum") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.SimpleEnum](
        "schemas/Enum_SimpleEnum.json",
        upickletest.SimpleEnum.A,
        """"A""""
      )
    }
    test("Enum_ColorEnum") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.ColorEnum](
        "schemas/Enum_ColorEnum.json",
        upickletest.ColorEnum.Mix(12345),
        """{"$type":"Mix","mix":12345}"""
      )
    }
    test("Enum_Enclosing") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.Enclosing](
        "schemas/Enum_Enclosing.json",
        upickletest.Enclosing("test", upickletest.SimpleEnum.A, Some(upickletest.SimpleEnum.B)),
        """{"str":"test","simple1":"A","simple2":"B"}"""
      )
    }
    test("Enum_LinkedList_Int") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.LinkedList[Int]](
        "schemas/Enum_LinkedList_Int.json",
        upickletest.LinkedList.Node2(1, upickletest.LinkedList.Node(2, upickletest.LinkedList.End)),
        """{"$type":"Node2","value":1,"next":{"$type":"Node","value":2,"next":"End"}}"""
      )
    }
    test("Enum_Domain") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.Domain](
        "schemas/Enum_Domain.json",
        upickletest.Domain.`reddit.com`,
        """"reddit.com""""
      )
    }
    test("Enum_ADomain") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.ADomain](
        "schemas/Enum_ADomain.json",
        upickletest.ADomain(upickletest.Domain.Something),
        """{"d":"Something"}"""
      )
    }

    test("NamedTuples_Example") {
      import upickle.implicits.namedTuples.default.given
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[NamedTupleExample](
        "schemas/NamedTuples_Example.json",
        (foo = Seq(1, 2, 3), bar = "hello", qux = Some(42)),
        """{"foo":[1,2,3],"bar":"hello","qux":42}"""
      )
    }
    test("NamedTuples_Schema") {
      import upickle.implicits.namedTuples.default.given
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[NamedTupleSchema](
        "schemas/NamedTuples_Schema.json",
        (
          (x = 23, y = 7.5, z = 500000000000L),
          (name = "Alice", isHuman = true, isAlien = false),
          (arr = Seq(1, 2, 3), optionalAny = None, optionalInt = Some(42))
        ),
        """[{"x":23,"y":7.5,"z":500000000000},{"name":"Alice","isHuman":true,"isAlien":false},{"arr":[1,2,3],"optionalAny":null,"optionalInt":42}]"""
      )
    }
    test("NamedTuples_MissingKeyShape") {
      import upickle.implicits.namedTuples.default.given
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[NamedTupleMissingKeyShape](
        "schemas/NamedTuples_MissingKeyShape.json",
        (foo = true),
        """{"foo":true}"""
      )
    }
  }
}
