package ujson
import utest._
object NonUtf8Tests extends TestSuite {
  def tests = Tests{

    test("nonUtf8") {
      val utf8Bytes = Array[Byte](34, -48, -65, -47, -128, -48, -72, -48, -78, -48, -75, -47, -126, 34)
      val utf8String = ujson.Str("привет")
      ujson.read(utf8Bytes) ==> utf8String
      ujson.writeToByteArray(utf8String) ==> utf8Bytes
    }
  }
}
