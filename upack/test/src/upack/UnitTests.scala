package upack
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import utest._

object UnitTests extends TestSuite{
  val tests = Tests {

    test("trivial"){
      val msg = Arr(Str("a"))
      val written = upack.write(msg)
      upack.read(written: geny.Readable) ==> msg
    }
    test("array"){
      val msg = Arr(
        Str("a"), Str("bb"), Str("ccc"), Str("dddd"), Str("eeeee"), Str("ffffff"),
        Str("g"), Str("hh"), Str("iii"), Str("jjjj"), Str("kkkkk"), Str("llllll"),
        Str("m"), Str("nn"), Str("ooo"), Str("pppp"), Str("qqqqq"), Str("rrrrrr"),
      )
      val written = upack.write(msg)
      val reader = new InputStreamMsgPackReader(new ByteArrayInputStream(written), 2, 2)
      val read = reader.parse(upack.Msg)

      read ==> msg
      reader.getBufferGrowCount ==> 2
      reader.getBufferCopyCount ==> 8
      reader.getBufferLength ==> 16
    }
    test("map"){
      val msg = Obj(
        Str("a") -> Int32(123), Str("c") -> Int32(456), Str("d") -> Int32(789),
        Str("e") -> Int32(123), Str("f") -> Int32(456), Str("g") -> Int32(789),
        Str("h") -> Int32(123), Str("i") -> Int32(456), Str("j") -> Int32(789),
        Str("k") -> Int32(123), Str("l") -> Int32(456), Str("m") -> Int32(789),
        Str("n") -> Int32(123), Str("o") -> Int32(456), Str("p") -> Int32(789),
        Str("q") -> Int32(123), Str("r") -> Int32(456), Str("s") -> Int32(789),
      )
      val written = upack.write(msg)
      val reader = new InputStreamMsgPackReader(new ByteArrayInputStream(written), 2, 2)
      val read = reader.parse(upack.Msg)

      read ==> msg
      reader.getBufferGrowCount ==> 2
      reader.getBufferCopyCount ==> 8
      reader.getBufferLength ==> 16
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
