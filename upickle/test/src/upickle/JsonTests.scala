package upickle
import utest._

object JsonTests extends TestSuite {
  val tests = Tests {
    val ugly =
      """
        |[
        |    "JSON Test Pattern pass1",
        |    {"object with 1 member":["array with 1 element"]},
        |    {},
        |    [],
        |    -42,
        |    true,
        |    false,
        |    null,
        |    {
        |        "integer": 1234567890,
        |        "real": -9876.543210,
        |        "e": 0.123456789e-12,
        |        "E": 1.234567890E+34,
        |        "":  23456789012E66,
        |        "zero": 0,
        |        "one": 1,
        |        "space": " ",
        |        "quote": "\"",
        |        "backslash": "\\",
        |        "controls": "\b\f\n\r\t",
        |        "slash": "/ & \/",
        |        "alpha": "abcdefghijklmnopqrstuvwyz",
        |        "ALPHA": "ABCDEFGHIJKLMNOPQRSTUVWYZ",
        |        "digit": "0123456789",
        |        "0123456789": "digit",
        |        "special": "`1~!@#$%^&*()_+-={':[,]}|;.</>?",
        |        "hex": "\u0123\u4567\u89AB\uCDEF\uabcd\uef4A",
        |        "true": true,
        |        "false": false,
        |        "null": null,
        |        "array":[  ],
        |        "object":{  },
        |        "address": "50 St. James Street",
        |        "url": "http://www.JSON.org/",
        |        "comment": "// /* <!-- --",
        |        "# -- --> */": " ",
        |        " s p a c e d " :[1,2 , 3
        |
        |,
        |
        |4 , 5        ,          6           ,7        ],"compact":[1,2,3,4,5,6,7],
        |        "jsontext": "{\"object with 1 member\":[\"array with 1 element\"]}",
        |        "quotes": "&#34; \u005Cu0022 %22 0x22 034 &#x22;",
        |        "\/\\\"\uCAFE\uBABE\uAB98\uFCDE\ubcda\uef4A\b\f\n\r\t`1~!@#$%^&*()_+-=[]{}|;:',./<>?"
        |: "A key can be any string"
        |    },
        |    0.5 ,98.6
        |,
        |99.44
        |,
        |
        |1066,
        |1e1,
        |0.1e1,
        |1e-1,
        |1e00,2e+00,2e-00
        |,"rosebud"]
      """.stripMargin
    val parsed = ujson.read(ugly)

    test("correctness"){
      val unparsed = ujson.write(parsed)
      val reparsed = ujson.read(unparsed)
      for (json <- Seq(parsed, reparsed)){
        assert(
          json(0).value == "JSON Test Pattern pass1",
          json(8)("real").value == -9876.54321,
          json(8)("comment").value == "// /* <!-- --",
          json(8)("jsontext").value == "{\"object with 1 member\":[\"array with 1 element\"]}",
          json(19).value == "rosebud"
        )
      }
      (parsed(19), reparsed(19))
    }
    test("inputs"){
      val unparsed = ujson.write(parsed)
      val fromString = ujson.read(unparsed)
      val fromBytes = ujson.read(unparsed.getBytes)
      val fromInputStream = ujson.read(new java.io.ByteArrayInputStream(unparsed.getBytes))

      assert(fromString == fromBytes)
      assert(fromBytes == fromInputStream)
    }
    test("shortcuts"){
      test("positive"){
        ujson.read("[1]").arr        ==> Seq(ujson.Num(1))
        ujson.read("1").num          ==> 1
        ujson.read("\"1\"").str      ==> "1"
        ujson.read("{\"1\": 1}").obj ==> Map("1" -> ujson.Num(1))
      }
      test("negative"){
        intercept[ujson.Value.InvalidData]{ujson.read("[1]").obj}
        intercept[ujson.Value.InvalidData]{ujson.read("1").obj}
        intercept[ujson.Value.InvalidData]{ujson.read("\"1\"").obj}

      }
    }
    test("writeBytesTo"){
      val out = new java.io.ByteArrayOutputStream()
      parsed.writeBytesTo(out)
      val s = new String(out.toByteArray)
      val parsedAgain = ujson.read(s)
      assert(parsed == parsedAgain)
    }
    test("writeBytesToSmall"){
      val out = new java.io.ByteArrayOutputStream()
      val parsed = ujson.Str("쫾")
      parsed.writeBytesTo(out)
      val s = new String(out.toByteArray)
      val parsedAgain = ujson.read(s)
      assert(parsed == parsedAgain)
    }
  }
}
