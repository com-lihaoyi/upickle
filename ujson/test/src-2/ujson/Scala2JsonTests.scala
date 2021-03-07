package ujson

import java.nio.charset.StandardCharsets

import utest._

object Scala2JsonTests extends TestSuite {
  val tests = Tests {
    test {
      val unparsed = """"\\\uCAFE""""
      val fromString = ujson.read(unparsed)
      val fromBytes = ujson.read(unparsed.getBytes)
      assert(fromString == fromBytes)
    }

    test {
      val unparsed = """"\/\\\"\uCAFE\uBABE\uAB98\uFCDE\ubcda\uef4A\b\f\n\r\t`1~!@#$%^&*()_+-=[]{}|;:',./<>?""""
      val fromString = ujson.read(unparsed)
      val fromBytes = ujson.read(unparsed.getBytes)
      assert(fromString == fromBytes)
    }
  }
}
