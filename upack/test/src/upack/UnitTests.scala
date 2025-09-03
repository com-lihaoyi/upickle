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
        Str("m"), Str("nn"), Str("ooo"), Str("pppp"), Str("qqqqq"), Str("rrrrrr")
      )
      val written = upack.write(msg)
      val reader = new InputStreamMsgPackReader(new ByteArrayInputStream(written), 2, 2)
      val read = reader.parse(upack.Msg)

      read ==> msg
      reader.getBufferGrowCount() ==> 2
      reader.getBufferCopyCount() ==> 8
      reader.getBufferLength() ==> 16
    }
    test("map"){
      val msg = Obj(
        Str("a") -> Int32(123), Str("c") -> Int32(456), Str("d") -> Int32(789),
        Str("e") -> Int32(123), Str("f") -> Int32(456), Str("g") -> Int32(789),
        Str("h") -> Int32(123), Str("i") -> Int32(456), Str("j") -> Int32(789),
        Str("k") -> Int32(123), Str("l") -> Int32(456), Str("m") -> Int32(789),
        Str("n") -> Int32(123), Str("o") -> Int32(456), Str("p") -> Int32(789),
        Str("q") -> Int32(123), Str("r") -> Int32(456), Str("s") -> Int32(789)
      )
      val written = upack.write(msg)
      val reader = new InputStreamMsgPackReader(new ByteArrayInputStream(written), 2, 2)
      val read = reader.parse(upack.Msg)

      read ==> msg
      reader.getBufferGrowCount() ==> 1
      reader.getBufferCopyCount() ==> 13
      reader.getBufferLength() ==> 8
    }
    test("compositeKeys"){
      val msg = Obj(Arr(Int32(1), Int32(2)) -> Int32(1))
      val written = upack.write(msg)
      val writtenStr = upickle.core.ParseUtils.bytesToString(written)
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
    test("extInMap"){
      val msg = Obj(Str("foo") -> Ext(33, new Array[Byte](12)), Str("bar") -> Null)
      val bytes1 = upack.write(msg)
      val parsed = upack.read(bytes1)
      val bytes2 = upack.write(parsed)
      assert(bytes1 sameElements bytes2)
    }
    test("extInList"){
      val msg = Arr(upack.Ext(33, new Array[Byte](4)), upack.False)
      val bytes1 = upack.write(msg)
      val parsed = upack.read(bytes1)
      val bytes2 = upack.write(parsed)
      assert(bytes1 sameElements bytes2)
    }
    test("reader.parse until EOF"){
      val msgs = List(upack.Int32(1), upack.Int32(2), upack.Int32(3), upack.Int32(4))
      val out = new ByteArrayOutputStream()
      msgs.foreach(_.writeBytesTo(out))
      val bytes = out.toByteArray()
      val in = new ByteArrayInputStream(bytes)
      val reader = new upack.InputStreamMsgPackReader(in)
      def readOne(): upack.Msg =
        reader.parse(upack.Msg)

      readOne() ==> upack.Int32(1)
      readOne() ==> upack.Int32(2)
      readOne() ==> upack.Int32(3)
      readOne() ==> upack.Int32(4)
      intercept[java.io.EOFException]{
        readOne()
      }
    }
    test("truncated input EOF"){
      val msg = upack.Arr(upack.Int32(1), upack.Int32(2), upack.Int32(3), upack.Int32(4))
      val bytes = upack.write(msg)
      val truncatedBytes = bytes.take(2)

      // upack.MsgPackReader has its own growBuffer override that throws
      test("upack.MsgPackReader"){
        intercept[java.io.EOFException]{
          upack.read(truncatedBytes) ==> msg
        }
      }
      // upack.InputStreamMsgPackReader uses BufferingElemParser semantics
      test("upack.InputStreamMsgPackReader"){
        intercept[java.io.EOFException]{
          val in = new ByteArrayInputStream(truncatedBytes)
          upack.read(in) ==> msg
        }
      }
    }
  }
}
