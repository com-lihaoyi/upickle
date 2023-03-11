package ujson

import java.nio.charset.StandardCharsets

import utest._

object Scala2JsonTests extends TestSuite {
  val tests = Tests {
    test {
      val unparsed = """"\\쫾""""
      val fromString = ujson.read(unparsed)
      val fromBytes = ujson.read(unparsed.getBytes)
      assert(fromString == fromBytes)
    }

    test {
      val unparsed = """"\/\\\"쫾몾ꮘﳞ볚\b\f\n\r\t`1~!@#$%^&*()_+-=[]{}|;:',./<>?""""
      val fromString = ujson.read(unparsed)
      val fromBytes = ujson.read(unparsed.getBytes)
      assert(fromString == fromBytes)
    }
  }
}
