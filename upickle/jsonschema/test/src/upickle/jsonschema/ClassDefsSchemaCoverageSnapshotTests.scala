package upickle.jsonschema

import utest.*

object ClassDefsSchemaCoverageSnapshotTests extends TestSuite {
  val tests = Tests {
    test("ClassDefs_ADTs_ADT0") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.ADTs.ADT0](
        "schemas/ClassDefs_ADTs_ADT0.json",
        upickletest.ADTs.ADT0(),
        "{}",
        """[]""",
        "object expected"
      )
    }
    test("ClassDefs_ADTs_ADTa") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.ADTs.ADTa](
        "schemas/ClassDefs_ADTs_ADTa.json",
        upickletest.ADTs.ADTa(1),
        """{"i":1}""",
        """{"i":"1"}""",
        "integer expected"
      )
    }
    test("ClassDefs_ADTs_ADTb") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.ADTs.ADTb](
        "schemas/ClassDefs_ADTs_ADTb.json",
        upickletest.ADTs.ADTb(1, "x"),
        """{"i":1,"s":"x"}""",
        """{"i":"1","s":"x"}""",
        "integer expected"
      )
    }
    test("ClassDefs_ADTs_ADTc") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.ADTs.ADTc](
        "schemas/ClassDefs_ADTs_ADTc.json",
        upickletest.ADTs.ADTc(1, "x", (1.1, 2.2)),
        """{"i":1,"s":"x","t":[1.1,2.2]}""",
        """{"i":1,"s":"x","t":[1.1,"2.2"]}""",
        "number expected"
      )
    }
    test("ClassDefs_ADTs_ADTd") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.ADTs.ADTd](
        "schemas/ClassDefs_ADTs_ADTd.json",
        upickletest.ADTs.ADTd(1, "x", (1.1, 2.2), upickletest.ADTs.ADTa(7)),
        """{"i":1,"s":"x","t":[1.1,2.2],"a":{"i":7}}""",
        """{"i":1,"s":"x","t":[1.1,2.2],"a":{"i":"7"}}""",
        "integer expected"
      )
    }
    test("ClassDefs_ADTs_ADTe") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.ADTs.ADTe](
        "schemas/ClassDefs_ADTs_ADTe.json",
        upickletest.ADTs.ADTe(1, "x", (1.1, 2.2), upickletest.ADTs.ADTa(7), Seq(1.1, 2.2)),
        """{"i":1,"s":"x","t":[1.1,2.2],"a":{"i":7},"q":[1.1,2.2]}""",
        """{"i":1,"s":"x","t":[1.1,2.2],"a":{"i":7},"q":["1.1",2.2]}""",
        "number expected"
      )
    }
    test("ClassDefs_ADTs_ADTf") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.ADTs.ADTf](
        "schemas/ClassDefs_ADTs_ADTf.json",
        upickletest.ADTs.ADTf(1, "x", (1.1, 2.2), upickletest.ADTs.ADTa(7), Seq(1.1, 2.2), Some(Some(true))),
        """{"i":1,"s":"x","t":[1.1,2.2],"a":{"i":7},"q":[1.1,2.2],"o":[true]}""",
        """{"i":1,"s":"x","t":[1.1,2.2],"a":{"i":7},"q":[1.1,2.2],"o":["true"]}""",
        "boolean expected"
      )
    }
    test("ClassDefs_ADTs_ADTz") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.ADTs.ADTz](
        "schemas/ClassDefs_ADTs_ADTz.json",
        upickletest.ADTs.ADTz(1, "a", 2, "b", 3, "c", 4, "d", 5, "e", 6, "f", 7, "g", 8, "h", 9, "i"),
        """{"t1":1,"t2":"a","t3":2,"t4":"b","t5":3,"t6":"c","t7":4,"t8":"d","t9":5,"t10":"e","t11":6,"t12":"f","t13":7,"t14":"g","t15":8,"t16":"h","t17":9,"t18":"i"}""",
        """{"t1":"1","t2":"a","t3":2,"t4":"b","t5":3,"t6":"c","t7":4,"t8":"d","t9":5,"t10":"e","t11":6,"t12":"f","t13":7,"t14":"g","t15":8,"t16":"h","t17":9,"t18":"i"}""",
        "integer expected"
      )
    }
    test("ClassDefs_Defaults_ADTa") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.Defaults.ADTa](
        "schemas/ClassDefs_Defaults_ADTa.json",
        upickletest.Defaults.ADTa(),
        "{}",
        """{"i":"0"}""",
        "integer expected"
      )
    }
    test("ClassDefs_Defaults_ADTb") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.Defaults.ADTb](
        "schemas/ClassDefs_Defaults_ADTb.json",
        upickletest.Defaults.ADTb(s = "x"),
        """{"s":"x"}""",
        """{"s":1}""",
        "string expected"
      )
    }
    test("ClassDefs_Defaults_ADTc") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.Defaults.ADTc](
        "schemas/ClassDefs_Defaults_ADTc.json",
        upickletest.Defaults.ADTc(s = "x"),
        """{"s":"x"}""",
        """{"s":"x","t":[1,"2"]}""",
        "number expected"
      )
    }
    test("ClassDefs_C1") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.C1](
        "schemas/ClassDefs_C1.json",
        upickletest.C1("n", List("a", "b")),
        """{"name":"n","types":["a","b"]}""",
        """{"name":1,"types":["a","b"]}""",
        "string expected"
      )
    }
    test("ClassDefs_C2") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.C2](
        "schemas/ClassDefs_C2.json",
        upickletest.C2(List(upickletest.C1("n", List("a")))),
        """{"results":[{"name":"n","types":["a"]}]}""",
        """{"results":"oops"}""",
        "array expected"
      )
    }
    test("ClassDefs_Result2") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.Result2](
        "schemas/ClassDefs_Result2.json",
        upickletest.Result2("n", "w", List("a")),
        """{"name":"n","whatever":"w","types":["a"]}""",
        """{"name":"n","whatever":"w","types":"a"}""",
        "array expected"
      )
    }
    test("ClassDefs_GeoCoding2") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.GeoCoding2](
        "schemas/ClassDefs_GeoCoding2.json",
        upickletest.GeoCoding2(List(upickletest.Result2("n", "w", List("a"))), "OK"),
        """{"results":[{"name":"n","whatever":"w","types":["a"]}],"status":"OK"}""",
        """{"results":[{"name":"n","whatever":"w","types":["a"]}],"status":200}""",
        "string expected"
      )
    }
  }
}
