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
          |      "additionalProperties": false
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
          |      "additionalProperties": false
          |    }
          |  },
          |  "$ref": "#/$defs/upickle.jsonschema.Person"
          |}""".stripMargin
      assert(rendered == expected)
    }

    test("recursiveCaseClass") {
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
          |            }
          |          ]
          |        }
          |      },
          |      "required": [
          |        "value",
          |        "next"
          |      ],
          |      "additionalProperties": false
          |    }
          |  },
          |  "$ref": "#/$defs/upickle.jsonschema.Node"
          |}""".stripMargin
      assert(rendered == expected)
    }

    test("recursiveEnum") {
      val rendered = upickle.default.schema[LinkedList[Int]].render(indent = 2)
      val expected =
        """{
          |  "$schema": "https://json-schema.org/draft/2020-12/schema",
          |  "$defs": {
          |    "upickle.jsonschema.LinkedList.End": {
          |      "type": "object",
          |      "properties": {},
          |      "required": [],
          |      "additionalProperties": false
          |    },
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
          |      "additionalProperties": false
          |    },
          |    "upickle.jsonschema.LinkedList[scala.Int]": {
          |      "oneOf": [
          |        {
          |          "$ref": "#/$defs/upickle.jsonschema.LinkedList.End"
          |        },
          |        {
          |          "$ref": "#/$defs/upickle.jsonschema.LinkedList.Cons[scala.Int]"
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
