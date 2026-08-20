package upickle.jsonschema

import upickle.default.*
import utest.*

case class Address(street: String, zip: Int) derives ReadWriter
case class Person(name: String, address: Address) derives ReadWriter

case class Node(value: Int, next: Option[Node]) derives ReadWriter

enum LinkedList[+T] derives ReadWriter:
  case End
  case Cons(value: T, next: LinkedList[T])

case class RequiredFields(i: Int, s: String = "x") derives ReadWriter
@upickle.implicits.allowUnknownKeys(false)
case class StrictUnknownKeys(i: Int) derives ReadWriter
case class NumericStringFidelity(bigI: BigInt, bigD: BigDecimal, l: Long) derives ReadWriter
case class IntKeyMap(m: Map[Int, String]) derives ReadWriter
case class FlattenInner(a: String, b: Int) derives ReadWriter
case class FlattenOuter(d: Double, @upickle.implicits.flatten inner: FlattenInner) derives ReadWriter
case class FlattenInnerWithDefault(a: Int, b: String = "x") derives ReadWriter
case class FlattenRequiredOuter(@upickle.implicits.flatten inner: FlattenInnerWithDefault) derives ReadWriter

@upickle.implicits.key("kind")
sealed trait KeyedTagBase derives ReadWriter
object KeyedTagBase:
  case class Foo(i: Int) extends KeyedTagBase
  case object Bar extends KeyedTagBase

sealed trait PlainTagBase derives ReadWriter
object PlainTagBase:
  case class Foo(i: Int) extends PlainTagBase
  case object Bar extends PlainTagBase

given JsonSchema[Address] = JsonSchema.derived
given JsonSchema[Person] = JsonSchema.derived
lazy given JsonSchema[Node] = JsonSchema.derived
given JsonSchema[LinkedList[Int]] = JsonSchema.derived
given JsonSchema[KeyedTagBase] = JsonSchema.derived
given JsonSchema[PlainTagBase] = JsonSchema.derived
given JsonSchema[RequiredFields] = JsonSchema.derived
given JsonSchema[StrictUnknownKeys] = JsonSchema.derived
given JsonSchema[NumericStringFidelity] = JsonSchema.derived
given JsonSchema[IntKeyMap] = JsonSchema.derived
given JsonSchema[FlattenInner] = JsonSchema.derived
given JsonSchema[FlattenOuter] = JsonSchema.derived
given JsonSchema[FlattenInnerWithDefault] = JsonSchema.derived
given JsonSchema[FlattenRequiredOuter] = JsonSchema.derived

object JsonSchemaTests extends TestSuite {
  val tests = Tests {
    test("nestedDefinitions") {
      SchemaSnapshotTestUtils.assertSerializationValidatesSchema[Person](
        Person("Bob", Address("Main", 12345)),
        """{"name":"Bob","address":{"street":"Main","zip":12345}}"""
      )

      val rendered = upickle.default.schema[Person].render(indent = 2)
      val expected =
        """{
          |  "$schema": "https://json-schema.org/draft/2020-12/schema",
          |  "$defs": {
          |    "upickle.jsonschema.Address": {
          |      "type": "object",
          |      "properties": {
          |        "street": {
          |          "type": "string"
          |        },
          |        "zip": {
          |          "type": "integer"
          |        }
          |      },
          |      "required": [
          |        "street",
          |        "zip"
          |      ],
          |      "additionalProperties": true
          |    },
          |    "upickle.jsonschema.Person": {
          |      "type": "object",
          |      "properties": {
          |        "name": {
          |          "type": "string"
          |        },
          |        "address": {
          |          "$ref": "#/$defs/upickle.jsonschema.Address"
          |        }
          |      },
          |      "required": [
          |        "name",
          |        "address"
          |      ],
          |      "additionalProperties": true
          |    }
          |  },
          |  "$ref": "#/$defs/upickle.jsonschema.Person"
          |}""".stripMargin
      assert(rendered == expected)
    }

    test("recursiveCaseClass") {
      SchemaSnapshotTestUtils.assertSerializationValidatesSchema[Node](
        Node(1, Some(Node(2, None))),
        """{"value":1,"next":{"value":2,"next":null}}"""
      )

      val rendered = upickle.default.schema[Node].render(indent = 2)
      val expected =
        """{
          |  "$schema": "https://json-schema.org/draft/2020-12/schema",
          |  "$defs": {
          |    "upickle.jsonschema.Node": {
          |      "type": "object",
          |      "properties": {
          |        "value": {
          |          "type": "integer"
          |        },
          |        "next": {
          |          "anyOf": [
          |            {
          |              "$ref": "#/$defs/upickle.jsonschema.Node"
          |            },
          |            {
          |              "type": "null"
          |            },
          |            {
          |              "type": "array",
          |              "minItems": 0,
          |              "maxItems": 1,
          |              "items": {
          |                "$ref": "#/$defs/upickle.jsonschema.Node"
          |              }
          |            }
          |          ]
          |        }
          |      },
          |      "required": [
          |        "value",
          |        "next"
          |      ],
          |      "additionalProperties": true
          |    }
          |  },
          |  "$ref": "#/$defs/upickle.jsonschema.Node"
          |}""".stripMargin
      assert(rendered == expected)
    }

    test("recursiveEnum") {
      SchemaSnapshotTestUtils.assertSerializationValidatesSchema[LinkedList[Int]](
        LinkedList.Cons(1, LinkedList.Cons(2, LinkedList.End)),
        """{"$type":"Cons","value":1,"next":{"$type":"Cons","value":2,"next":"End"}}"""
      )

      val rendered = upickle.default.schema[LinkedList[Int]].render(indent = 2)
      val expected =
        """{
          |  "$schema": "https://json-schema.org/draft/2020-12/schema",
          |  "$defs": {
          |    "upickle.jsonschema.LinkedList.Cons[scala.Int]": {
          |      "type": "object",
          |      "properties": {
          |        "value": {
          |          "type": "integer"
          |        },
          |        "next": {
          |          "$ref": "#/$defs/upickle.jsonschema.LinkedList[scala.Int]"
          |        }
          |      },
          |      "required": [
          |        "value",
          |        "next"
          |      ],
          |      "additionalProperties": true
          |    },
          |    "upickle.jsonschema.LinkedList[scala.Int]": {
          |      "oneOf": [
          |        {
          |          "const": "End"
          |        },
          |        {
          |          "allOf": [
          |            {
          |              "$ref": "#/$defs/upickle.jsonschema.LinkedList.Cons[scala.Int]"
          |            },
          |            {
          |              "type": "object",
          |              "properties": {
          |                "$type": {
          |                  "const": "Cons"
          |                }
          |              },
          |              "required": [
          |                "$type"
          |              ]
          |            }
          |          ]
          |        }
          |      ]
          |    }
          |  },
          |  "$ref": "#/$defs/upickle.jsonschema.LinkedList[scala.Int]"
          |}""".stripMargin
      assert(rendered == expected)
    }

    test("tagKeyOverrideAndShortTags") {
      SchemaSnapshotTestUtils.assertSerializationValidatesSchema[KeyedTagBase](
        KeyedTagBase.Foo(1),
        """{"kind":"Foo","i":1}"""
      )
      SchemaSnapshotTestUtils.assertSerializationValidatesSchema[KeyedTagBase](
        KeyedTagBase.Bar,
        """"Bar""""
      )
      SchemaSnapshotTestUtils.assertJsonDoesNotValidateSchema[KeyedTagBase](
        """{"$type":"Foo","i":1}"""
      )
      SchemaSnapshotTestUtils.assertJsonDoesNotValidateSchema[KeyedTagBase](
        """{"kind":"upickle.jsonschema.KeyedTagBase.Foo","i":1}"""
      )
    }

    test("plainShortTagRegression") {
      SchemaSnapshotTestUtils.assertSerializationValidatesSchema[PlainTagBase](
        PlainTagBase.Foo(1),
        """{"$type":"Foo","i":1}"""
      )
      SchemaSnapshotTestUtils.assertSerializationValidatesSchema[PlainTagBase](
        PlainTagBase.Bar,
        """"Bar""""
      )
      SchemaSnapshotTestUtils.assertJsonDoesNotValidateSchema[PlainTagBase](
        """{"$type":"upickle.jsonschema.PlainTagBase.Foo","i":1}"""
      )
    }

    test("requiredFieldsRespected") {
      SchemaSnapshotTestUtils.assertSerializationValidatesSchema[RequiredFields](
        RequiredFields(1),
        """{"i":1}"""
      )
      SchemaSnapshotTestUtils.assertJsonDoesNotValidateSchemaWithMessage[RequiredFields](
        """{"s":"x"}""",
        "required property 'i' not found"
      )
    }

    test("unknownKeysRespected") {
      SchemaSnapshotTestUtils.assertSerializationValidatesSchema[StrictUnknownKeys](
        StrictUnknownKeys(1),
        """{"i":1}"""
      )
      SchemaSnapshotTestUtils.assertJsonDoesNotValidateSchemaWithMessage[StrictUnknownKeys](
        """{"i":1,"extra":2}""",
        "property 'extra' is not defined"
      )
    }

    test("numericStringFidelity") {
      SchemaSnapshotTestUtils.assertSerializationValidatesSchema[NumericStringFidelity](
        NumericStringFidelity(BigInt("12345678901234567890"), BigDecimal("123.456"), 9007199254740993L),
        """{"bigI":"12345678901234567890","bigD":"123.456","l":"9007199254740993"}"""
      )
      SchemaSnapshotTestUtils.assertJsonDoesNotValidateSchemaWithMessage[NumericStringFidelity](
        """{"bigI":123,"bigD":"123.456","l":"9007199254740993"}""",
        "string expected"
      )
      SchemaSnapshotTestUtils.assertJsonDoesNotValidateSchemaWithMessage[NumericStringFidelity](
        """{"bigI":"123","bigD":"123.456","l":"not-long"}""",
        "does not match the regex pattern"
      )
    }

    test("mapKeyTyping") {
      SchemaSnapshotTestUtils.assertSerializationValidatesSchema[IntKeyMap](
        IntKeyMap(scala.collection.immutable.ListMap(1 -> "one", 2 -> "two")),
        """{"m":{"1":"one","2":"two"}}"""
      )
      SchemaSnapshotTestUtils.assertJsonDoesNotValidateSchemaWithMessage[IntKeyMap](
        """{"m":{"x":"one"}}""",
        "does not match the regex pattern"
      )
    }

    test("flattenedCaseClassShape") {
      SchemaSnapshotTestUtils.assertSerializationValidatesSchema[FlattenOuter](
        FlattenOuter(1.5, FlattenInner("x", 7)),
        """{"d":1.5,"a":"x","b":7}"""
      )
      SchemaSnapshotTestUtils.assertJsonDoesNotValidateSchemaWithMessage[FlattenOuter](
        """{"d":1.5,"inner":{"a":"x","b":7}}""",
        "required property 'a' not found"
      )
      SchemaSnapshotTestUtils.assertJsonDoesNotValidateSchemaWithMessage[FlattenOuter](
        """{"d":1.5,"a":"x"}""",
        "required property 'b' not found"
      )
    }

    test("flattenedRequiredFromNestedDefaults") {
      SchemaSnapshotTestUtils.assertSerializationValidatesSchema[FlattenRequiredOuter](
        FlattenRequiredOuter(FlattenInnerWithDefault(1)),
        """{"a":1}"""
      )
      SchemaSnapshotTestUtils.assertJsonDoesNotValidateSchemaWithMessage[FlattenRequiredOuter](
        """{}""",
        "required property 'a' not found"
      )
    }
  }
}
