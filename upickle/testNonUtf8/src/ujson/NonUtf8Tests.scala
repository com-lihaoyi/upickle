package ujson
import utest._
object NonUtf8Tests extends TestSuite {
  def tests = Tests{

    test("json") {
      val utf8Bytes = Array[Byte](34, -48, -65, -47, -128, -48, -72, -48, -78, -48, -75, -47, -126, 34)
      val utf8String = ujson.Str("привет")
      ujson.read(utf8Bytes) ==> utf8String
      ujson.writeToByteArray(utf8String) ==> utf8Bytes
    }
    test("msgpack") {
      val utf8String = upack.Str("привет")
      val utf8Bytes = Array[Byte](-84, -48, -65, -47, -128, -48, -72, -48, -78, -48, -75, -47, -126)
      upack.read(utf8Bytes) ==> utf8String
      upack.writeToByteArray(utf8String) ==> utf8Bytes
    }
  }
}
