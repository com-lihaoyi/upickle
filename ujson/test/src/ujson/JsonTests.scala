package ujson
import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

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
        |        "hex": "ģ䕧覫췯ꯍ",
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
        |        "quotes": "&#34; \""".stripMargin + "\\" + """u0022 %22 0x22 034 &#x22;",
        |        "\/\\\"쫾몾ꮘﳞ볚\b\f\n\r\t`1~!@#$%^&*()_+-=[]{}|;:',./<>?"
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
    lazy val parsed = ujson.read(ugly)

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
      TestUtil.checkParse(ugly, true)
    }
    test("streaming"){
      val reader = new InputStreamParser[ujson.Value](
        new ByteArrayInputStream(ugly.getBytes(StandardCharsets.UTF_8)),
        2,
        2
      )
      val read = reader.parse(ujson.Value)

      read ==> ujson.read(ugly)

      // NOTE: there's a difference in the way Scala 2 and 3 handle unicode
      // characters. Under Scala 2, the length of the decoded string is 1412
      // bytes, whereas under Scala 3 it's 1454 bytes.
      assert(reader.getBufferGrowCount() <= 6)
      assert(reader.getBufferCopyCount() <= 31)
      assert(reader.getBufferLength() <= 256)
    }
    test("batch"){
      val bytes = ugly.getBytes(StandardCharsets.UTF_8)
      val reader = new ByteArrayParser[ujson.Value](bytes)
      val read = reader.parse(ujson.Value)

      read ==> ujson.read(ugly)

      reader.getBufferGrowCount() ==> 0
      reader.getBufferCopyCount() ==> 0
      reader.getBufferLength() ==> bytes.length
    }
    test("sortKeys"){
      val raw = """{"d": [{"c": 0, "b": 1}], "a": 2}"""
      val sorted = """{
                     |    "a": 2,
                     |    "d": [
                     |        {
                     |            "b": 1,
                     |            "c": 0
                     |        }
                     |    ]
                     |}""".stripMargin
      ujson.reformat(raw, indent = 4, sortKeys = true) ==> sorted
      
      ujson.write(ujson.read(raw), indent = 4, sortKeys = true) ==> sorted
      
      val baos = new java.io.ByteArrayOutputStream
      ujson.writeToOutputStream(ujson.read(raw), baos, indent = 4, sortKeys = true)
      baos.toString ==> sorted
      
      val writer = new java.io.StringWriter
      ujson.writeTo(ujson.read(raw), writer, indent = 4, sortKeys = true)
      writer.toString ==> sorted
      
      new String(ujson.writeToByteArray(ujson.read(raw), indent = 4, sortKeys = true)) ==> sorted
      
      new String(ujson.reformatToByteArray(raw, indent = 4, sortKeys = true)) ==> sorted
    }
  }
}
