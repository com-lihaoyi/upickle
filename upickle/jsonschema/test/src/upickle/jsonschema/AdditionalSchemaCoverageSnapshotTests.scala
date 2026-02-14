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
    test("Enum_SimpleEnum") { SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.SimpleEnum]("schemas/Enum_SimpleEnum.json") }
    test("Enum_ColorEnum") { SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.ColorEnum]("schemas/Enum_ColorEnum.json") }
    test("Enum_Enclosing") { SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.Enclosing]("schemas/Enum_Enclosing.json") }
    test("Enum_LinkedList_Int") { SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.LinkedList[Int]]("schemas/Enum_LinkedList_Int.json") }
    test("Enum_Domain") { SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.Domain]("schemas/Enum_Domain.json") }
    test("Enum_ADomain") { SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.ADomain]("schemas/Enum_ADomain.json") }

    test("NamedTuples_Example") { SchemaSnapshotTestUtils.assertSchemaSnapshot[NamedTupleExample]("schemas/NamedTuples_Example.json") }
    test("NamedTuples_Schema") { SchemaSnapshotTestUtils.assertSchemaSnapshot[NamedTupleSchema]("schemas/NamedTuples_Schema.json") }
    test("NamedTuples_MissingKeyShape") { SchemaSnapshotTestUtils.assertSchemaSnapshot[NamedTupleMissingKeyShape]("schemas/NamedTuples_MissingKeyShape.json") }
  }
}
