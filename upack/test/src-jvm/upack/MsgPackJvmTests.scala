package upack
import java.io.ByteArrayOutputStream

import upickle.core.Util
import utest._

import scala.collection.mutable
object MsgPackJvmTests extends TestSuite{
  def readBytes(path: String) = java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(path))
  def readMsgs(path: String) = {
    val bytes = readBytes(path)
    val output = mutable.ArrayBuffer.empty[Msg]
    val p = new MsgPackReader(0, bytes, Msg)
    while(p.index < bytes.length){
      output.append(p.parse())
    }
    upack.Arr(output)
  }
  val tests = Tests{
    'hello - {

      // Taken from:
      // https://github.com/msgpack/msgpack-ruby/tree/a22d8268f82e0f2ae95f038285af43ce5971810e/spec
      val casesJson = "upack/test/resources/cases.json"
      val casesMsg = "upack/test/resources/cases.msg"
      val casesCompactMsg = "upack/test/resources/cases_compact.msg"
      val expectedJson = ujson.read(readBytes(casesJson))
      val msg = readMsgs(casesMsg)
      val msgCompact = readMsgs(casesCompactMsg)
      val jsonMsg = upack.transform(msg, ujson.Js)
      val jsonMsgCompact = upack.transform(msgCompact, ujson.Js)
      val writtenMsg = Util.bytesToString(upack.write(msg))
      val rewrittenMsg = Util.bytesToString(
        upack.write(upack.read(upack.write(msg)))
      )
      val writtenMsgCompact = Util.bytesToString(upack.write(msgCompact))
      val rewrittenMsgCompact = Util.bytesToString(
        upack.write(upack.read(upack.write(msgCompact)))
      )
      assert(
        expectedJson == jsonMsg,
        expectedJson == jsonMsgCompact,
//        Still doesn't pass:
//
//        writtenMsg == rewrittenMsg,
//        writtenMsgCompact == rewrittenMsgCompact
      )
    }
  }
}