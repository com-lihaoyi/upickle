package upack
import java.io.ByteArrayOutputStream

import utest._

object UnitTests extends TestSuite{
  val tests = Tests {

    test("trivial"){
      val msg = Arr(Str("a"))
      val written = upack.write(msg)
      upack.read(written: geny.Readable) ==> msg
    }
    test("map"){
      val msg = Arr(Str("a"))
      val written = transform(msg, new MsgPackWriter().map(_.toByteArray)) // does map preserve array lengths?
      upack.read(written: geny.Readable) ==> msg
    }
    test("compositeKeys"){
      val msg = Obj(Arr(Int32(1), Int32(2)) -> Int32(1))
      val written = upack.write(msg)
      val writtenStr = upickle.core.Util.bytesToString(written)
      writtenStr ==> "81-92-01-02-01"

      upack.read(written) ==> msg


      intercept[upickle.core.Abort]{
        upack.transform(written, ujson.Value)
      }
      intercept[upickle.core.Abort] {
        upack.transform(msg, ujson.Value)
      }
    }
    test("writeBytesTo"){
      val msg = Obj(Arr(Int32(1), Int32(2)) -> Int32(1))
      val out = new ByteArrayOutputStream()
      msg.writeBytesTo(out)
      val bytes = out.toByteArray
      val parsed = upack.read(bytes)
      assert(msg == parsed)
    }
  }
}
