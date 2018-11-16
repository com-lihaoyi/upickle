package upack
import java.io.ByteArrayOutputStream

import utest._
object MsgPackTests extends TestSuite{
  val unitCases = {
    val arr = new ByteArrayOutputStream()
    val input = getClass.getResourceAsStream("msgpack-unit-test-suite.json")
    val buffer = new Array[Byte](4096)
    while ( {
      input.read(buffer) match{
        case -1 => false
        case n =>
          arr.write(buffer, 0, n)
          true
      }
    }) ()
    ujson.read(new String(arr.toByteArray))
  }

  val tests = Tests{
    'hello - {
      for{
        (k, v) <- unitCases.obj
        testCase <- v.arr
        packed0 <- testCase("msgpack").arr
        if k != "12.binary.yaml"
      }{
        val (tag, expectedValue) = testCase.obj.find{_._1 != "msgpack"}.get
        val packedStr = packed0.str
        println(k + " " + tag + " " + expectedValue + " " + packedStr)
        val packed = packedStr.split('-').map(Integer.parseInt(_, 16).toByte)

        val parsedValue = new MsgPackReader(0, packed, ujson.Js).parse()
        assert(parsedValue == expectedValue)
        val boas = new ByteArrayOutputStream()
        val out = new MsgPackWriter(boas)
      }
    }
  }
}