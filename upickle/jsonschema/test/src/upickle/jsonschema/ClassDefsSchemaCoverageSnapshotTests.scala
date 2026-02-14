package upickle.jsonschema

import utest.*

object ClassDefsSchemaCoverageSnapshotTests extends TestSuite {
  val tests = Tests {
    test("ClassDefs_ADTs_ADT0") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.ADTs.ADT0](
        "schemas/ClassDefs_ADTs_ADT0.json"
      )
    }
    test("ClassDefs_ADTs_ADTa") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.ADTs.ADTa](
        "schemas/ClassDefs_ADTs_ADTa.json"
      )
    }
    test("ClassDefs_ADTs_ADTb") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.ADTs.ADTb](
        "schemas/ClassDefs_ADTs_ADTb.json"
      )
    }
    test("ClassDefs_ADTs_ADTc") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.ADTs.ADTc](
        "schemas/ClassDefs_ADTs_ADTc.json"
      )
    }
    test("ClassDefs_ADTs_ADTd") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.ADTs.ADTd](
        "schemas/ClassDefs_ADTs_ADTd.json"
      )
    }
    test("ClassDefs_ADTs_ADTe") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.ADTs.ADTe](
        "schemas/ClassDefs_ADTs_ADTe.json"
      )
    }
    test("ClassDefs_ADTs_ADTf") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.ADTs.ADTf](
        "schemas/ClassDefs_ADTs_ADTf.json"
      )
    }
    test("ClassDefs_ADTs_ADTz") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.ADTs.ADTz](
        "schemas/ClassDefs_ADTs_ADTz.json"
      )
    }
    test("ClassDefs_Defaults_ADTa") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.Defaults.ADTa](
        "schemas/ClassDefs_Defaults_ADTa.json"
      )
    }
    test("ClassDefs_Defaults_ADTb") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.Defaults.ADTb](
        "schemas/ClassDefs_Defaults_ADTb.json"
      )
    }
    test("ClassDefs_Defaults_ADTc") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.Defaults.ADTc](
        "schemas/ClassDefs_Defaults_ADTc.json"
      )
    }
    test("ClassDefs_C1") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.C1](
        "schemas/ClassDefs_C1.json"
      )
    }
    test("ClassDefs_C2") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.C2](
        "schemas/ClassDefs_C2.json"
      )
    }
    test("ClassDefs_Result2") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.Result2](
        "schemas/ClassDefs_Result2.json"
      )
    }
    test("ClassDefs_GeoCoding2") {
      SchemaSnapshotTestUtils.assertSchemaSnapshot[upickletest.GeoCoding2](
        "schemas/ClassDefs_GeoCoding2.json"
      )
    }
  }
}
