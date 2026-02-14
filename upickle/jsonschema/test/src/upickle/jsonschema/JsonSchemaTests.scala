package upickle.jsonschema

import upickle.default.*
import utest.*

case class Address(street: String, zip: Int) derives ReadWriter
case class Person(name: String, address: Address) derives ReadWriter

case class Node(value: Int, next: Option[Node]) derives ReadWriter

enum LinkedList[+T] derives ReadWriter:
  case End
  case Cons(value: T, next: LinkedList[T])

given JsonSchema[Address] = JsonSchema.derived
given JsonSchema[Person] = JsonSchema.derived
lazy given JsonSchema[Node] = JsonSchema.derived
given JsonSchema[LinkedList[Int]] = JsonSchema.derived

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
          |      "required": [],
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
          |      "required": [],
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
          |      "required": [],
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
          |      "required": [],
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
  }
}
