package upickle.jsonschema

import scala.collection.immutable.ListMap
import utest.*

object MacroSchemaCoverageSnapshotTests extends TestSuite {
  val tests = Tests {
    test("Macro_SealedClass") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.SealedClass](
        "schemas/Macro_SealedClass.json",
        upickletest.SealedClass(3, "Hello"),
        """{"$type":"SealedClass","i":3,"s":"Hello"}""",
        """{"$type":"SealedClass","i":"3","s":"Hello"}""",
        "integer expected"
      )
    }
    test("Macro_KeyedPerson") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.KeyedPerson](
        "schemas/Macro_KeyedPerson.json",
        upickletest.KeyedPerson("A", "B"),
        """{"first_name":"A","last_name":"B"}""",
        """{"first_name":1,"last_name":"B"}""",
        "string expected"
      )
    }
    test("Macro_GenericIssue545_Person") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.GenericIssue545.Person](
        "schemas/Macro_GenericIssue545_Person.json",
        upickletest.GenericIssue545.Person(1, "bob"),
        """{"id":1,"name":"bob"}""",
        """{"id":"1","name":"bob"}""",
        "integer expected"
      )
    }
    test("Macro_GenericIssue545_ApiResult_Person") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[
        upickletest.GenericIssue545.ApiResult[upickletest.GenericIssue545.Person]
      ](
        "schemas/Macro_GenericIssue545_ApiResult_Person.json",
        upickletest.GenericIssue545.ApiResult(Some(upickletest.GenericIssue545.Person(1, "bob")), 2),
        """{"data":{"id":1,"name":"bob"},"total_count":2}""",
        """{"data":{"id":"1","name":"bob"},"total_count":2}""",
        "integer expected"
      )
    }
    test("Macro_UnknownKeys_Default") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.UnknownKeys.Default](
        "schemas/Macro_UnknownKeys_Default.json",
        upickletest.UnknownKeys.Default(1, "n"),
        """{"id":1,"name":"n"}""",
        """{"id":"1","name":"n"}""",
        "integer expected"
      )
    }
    test("Macro_UnknownKeys_DisAllow") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.UnknownKeys.DisAllow](
        "schemas/Macro_UnknownKeys_DisAllow.json",
        upickletest.UnknownKeys.DisAllow(1, "n"),
        """{"id":1,"name":"n"}""",
        """{"id":"1","name":"n"}""",
        "integer expected"
      )
    }
    test("Macro_UnknownKeys_Allow") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.UnknownKeys.Allow](
        "schemas/Macro_UnknownKeys_Allow.json",
        upickletest.UnknownKeys.Allow(1, "n"),
        """{"id":1,"name":"n"}""",
        """{"id":"1","name":"n"}""",
        "integer expected"
      )
    }
    test("Macro_Flatten_Nested") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.Flatten.Nested](
        "schemas/Macro_Flatten_Nested.json",
        upickletest.Flatten.Nested(3.0, ListMap("one" -> 1, "two" -> 2)),
        """{"d":3,"one":1,"two":2}""",
        """{"d":"3","one":1,"two":2}""",
        "number expected"
      )
    }
    test("Macro_Flatten_Nested2") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.Flatten.Nested2](
        "schemas/Macro_Flatten_Nested2.json",
        upickletest.Flatten.Nested2("hello"),
        """{"name":"hello"}""",
        """{"name":42}""",
        "string expected"
      )
    }
    test("Macro_Flatten_Outer") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.Flatten.Outer](
        "schemas/Macro_Flatten_Outer.json",
        upickletest.Flatten.Outer(1.1, upickletest.Flatten.Inner(upickletest.Flatten.InnerMost("test", 42), true)),
        """{"d":1.1,"a":"test","b":42,"c":true}""",
        """{"d":"1.1","a":"test","b":42,"c":true}""",
        "number expected"
      )
    }
    test("Macro_Flatten_FlattenWithDefault") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.Flatten.FlattenWithDefault](
        "schemas/Macro_Flatten_FlattenWithDefault.json",
        upickletest.Flatten.FlattenWithDefault(10, upickletest.Flatten.NestedWithDefault(l = "default")),
        """{"i":10,"l":"default"}""",
        """{"i":"10","l":"default"}""",
        "integer expected"
      )
    }
    test("Macro_Flatten_FlattenSeq") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.Flatten.FlattenSeq](
        "schemas/Macro_Flatten_FlattenSeq.json",
        upickletest.Flatten.FlattenSeq(Seq("a" -> 1, "b" -> 2)),
        """{"a":1,"b":2}""",
        """{"n":"oops","a":1,"b":2}""",
        "integer expected"
      )
    }
    test("Macro_Flatten_Collection") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.Flatten.Collection](
        "schemas/Macro_Flatten_Collection.json",
        upickletest.Flatten.Collection(
          scala.collection.mutable.LinkedHashMap("a" -> upickletest.Flatten.ValueClass(3.0), "b" -> upickletest.Flatten.ValueClass(4.0))
        ),
        """{"a":{"value":3},"b":{"value":4}}""",
        """{"n":"oops","a":{"value":3}}""",
        "object expected"
      )
    }
    test("Macro_Flatten_FlattenIntKey") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.Flatten.FlattenIntKey](
        "schemas/Macro_Flatten_FlattenIntKey.json",
        upickletest.Flatten.FlattenIntKey(10, ListMap(1 -> "one", 2 -> "two")),
        """{"i":10,"1":"one","2":"two"}""",
        """{"i":"10","1":"one","2":"two"}""",
        "integer expected"
      )
    }
    test("Macro_Flatten_FlattenSeqIntKey") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.Flatten.FlattenSeqIntKey](
        "schemas/Macro_Flatten_FlattenSeqIntKey.json",
        upickletest.Flatten.FlattenSeqIntKey(Seq(1 -> "one", 2 -> "two")),
        """{"1":"one","2":"two"}""",
        """{"n":"oops","1":"one","2":"two"}""",
        "does not match the regex pattern"
      )
    }
    test("Macro_Flatten_FlattenLongKey") {
      SchemaSnapshotTestUtils.assertSchemaSerializationCase[upickletest.Flatten.FlattenLongKey](
        "schemas/Macro_Flatten_FlattenLongKey.json",
        upickletest.Flatten.FlattenLongKey(ListMap(100L -> 1, 200L -> 2)),
        """{"100":1,"200":2}""",
        """{"m":"oops","100":1,"200":2}""",
        "integer expected"
      )
    }
  }
}
