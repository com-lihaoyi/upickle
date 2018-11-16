package upack
import java.io.ByteArrayOutputStream

import upickle.core.Util
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
//        if k != "12.binary.yaml"
        if k != "50.timestamp.yaml"
        if k != "60.ext.yaml"
        testCase <- v.arr
        packed0 <- testCase("msgpack").arr
      }{
        val (tag, expectedJson) = testCase.obj.find{_._1 != "msgpack"}.get
        val packedStr = packed0.str
        println(k + " " + tag + " " + expectedJson + " " + packedStr)
        val packed = Util.stringToBytes(packedStr)

        val jsonified = new MsgPackReader(0, packed, ujson.Js).parse()
        assert(jsonified == expectedJson)

        val msg = new MsgPackReader(0, packed, upack.Msg).parse()
        val boas = new ByteArrayOutputStream()

        val rewrittenBytes = Msg.transform(msg, new MsgPackWriter(boas)).toByteArray
        val rewritten = Util.bytesToString(rewrittenBytes)
        val possibilities = testCase("msgpack").arr.map(_.str)
        //        println(msg)
        assert(possibilities.contains(rewritten))
      }
    }
  }
}