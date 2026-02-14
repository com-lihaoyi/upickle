package upickle.jsonschema

import utest.*

object MacroSchemaCoverageSnapshotTests extends TestSuite {
  val tests = Tests {
    test("Macro_SealedClass") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.SealedClass](
        "schemas/Macro_SealedClass.json"
      )
    }
    test("Macro_KeyedPerson") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.KeyedPerson](
        "schemas/Macro_KeyedPerson.json"
      )
    }
    test("Macro_GenericIssue545_Person") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.GenericIssue545.Person](
        "schemas/Macro_GenericIssue545_Person.json"
      )
    }
    test("Macro_GenericIssue545_ApiResult_Person") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[
        upickletest.GenericIssue545.ApiResult[upickletest.GenericIssue545.Person]
      ](
        "schemas/Macro_GenericIssue545_ApiResult_Person.json"
      )
    }
    test("Macro_UnknownKeys_Default") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.UnknownKeys.Default](
        "schemas/Macro_UnknownKeys_Default.json"
      )
    }
    test("Macro_UnknownKeys_DisAllow") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.UnknownKeys.DisAllow](
        "schemas/Macro_UnknownKeys_DisAllow.json"
      )
    }
    test("Macro_UnknownKeys_Allow") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.UnknownKeys.Allow](
        "schemas/Macro_UnknownKeys_Allow.json"
      )
    }
    test("Macro_Flatten_Nested") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.Flatten.Nested](
        "schemas/Macro_Flatten_Nested.json"
      )
    }
    test("Macro_Flatten_Nested2") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.Flatten.Nested2](
        "schemas/Macro_Flatten_Nested2.json"
      )
    }
    test("Macro_Flatten_Outer") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.Flatten.Outer](
        "schemas/Macro_Flatten_Outer.json"
      )
    }
    test("Macro_Flatten_FlattenWithDefault") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.Flatten.FlattenWithDefault](
        "schemas/Macro_Flatten_FlattenWithDefault.json"
      )
    }
    test("Macro_Flatten_FlattenSeq") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.Flatten.FlattenSeq](
        "schemas/Macro_Flatten_FlattenSeq.json"
      )
    }
    test("Macro_Flatten_Collection") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.Flatten.Collection](
        "schemas/Macro_Flatten_Collection.json"
      )
    }
    test("Macro_Flatten_FlattenIntKey") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.Flatten.FlattenIntKey](
        "schemas/Macro_Flatten_FlattenIntKey.json"
      )
    }
    test("Macro_Flatten_FlattenSeqIntKey") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.Flatten.FlattenSeqIntKey](
        "schemas/Macro_Flatten_FlattenSeqIntKey.json"
      )
    }
    test("Macro_Flatten_FlattenLongKey") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.Flatten.FlattenLongKey](
        "schemas/Macro_Flatten_FlattenLongKey.json"
      )
    }
  }
}
