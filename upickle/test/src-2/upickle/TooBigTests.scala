package upickle

import java.io.{BufferedOutputStream, BufferedWriter, OutputStreamWriter}

import utest._
import upickle.legacy.read

object TooBigTests extends TestSuite {

  def tests = Tests {
    test("tooManyFields"){
      test("big63") {
        val b63 = Big63(
          0, 1, 2, 3, 4, 5, 6, 7,
          8, 9, 10, 11, 12, 13, 14,
          15, 16, 17, 18, 19, 20, 21,
          22, 23, 24, 25, 26, 27, 28,
          29, 30, 31, 32, 33, 34, 35,
          36, 37, 38, 39, 40, 41, 42,
          43, 44, 45, 46, 47, 48, 49,
          50, 51, 52, 53, 54, 55, 56,
          57, 58, 59, 60, 61, 62
        )
        val written63 = upickle.default.write(b63)
        assert(upickle.default.read[Big63](written63) == b63)
      }
      test("big64") {
        val b64 = Big64(
          0, 1, 2, 3, 4, 5, 6, 7,
          8, 9, 10, 11, 12, 13, 14,
          15, 16, 17, 18, 19, 20, 21,
          22, 23, 24, 25, 26, 27, 28,
          29, 30, 31, 32, 33, 34, 35,
          36, 37, 38, 39, 40, 41, 42,
          43, 44, 45, 46, 47, 48, 49,
          50, 51, 52, 53, 54, 55, 56,
          57, 58, 59, 60, 61, 62, 63
        )
        val written64 = upickle.default.write(b64)
        assert(upickle.default.read[Big64](written64) == b64)
      }
      test("big65") {
        val b65 = Big65(
          0, 1, 2, 3, 4, 5, 6, 7,
          8, 9, 10, 11, 12, 13, 14,
          15, 16, 17, 18, 19, 20, 21,
          22, 23, 24, 25, 26, 27, 28,
          29, 30, 31, 32, 33, 34, 35,
          36, 37, 38, 39, 40, 41, 42,
          43, 44, 45, 46, 47, 48, 49,
          50, 51, 52, 53, 54, 55, 56,
          57, 58, 59, 60, 61, 62, 63,
          64
        )
        val written65 = upickle.default.write(b65)
        assert(upickle.default.read[Big65](written65) == b65)
      }
      test("big150") {
        val b150 = Big150(
          0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
          10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
          20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
          30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
          40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
          50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
          60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
          70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
          80, 81, 82, 83, 84, 85, 86, 87, 88, 89,
          90, 91, 92, 93, 94, 95, 96, 97, 98, 99,
          100, 101, 102, 103, 104, 105, 106, 107, 108, 109,
          110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
          120, 121, 122, 123, 124, 125, 126, 127, 128, 129,
          130, 131, 132, 133, 134, 135, 136, 137, 138, 139,
          140, 141, 142, 143, 144, 145, 146, 147, 148, 149,
        )


        val written150 = upickle.default.write(b150)
        assert(upickle.default.read[Big150](written150) == b150)

        val err = intercept[upickle.core.AbortException] {
          upickle.default.read[Big150](
            written150
              .replace("\"_2\":2,", "")
              .replace("\"_52\":52,", "")
              .replace("\"_102\":102,", "")
              .replace("\"_142\":142,", "")
          )
        }
        assert(
          err.getMessage ==
            "missing keys in dictionary: _2, _52, _102, _142 at index 1392"
        )
      }
    }
//    test("hugeFile"){
//      import java.nio.file.{Files, Path}
//
//      val tmp = Files.createTempFile("big", ".json")
//      try {
//        val os = Files.newOutputStream(tmp)
//        val buf = new BufferedWriter(new OutputStreamWriter(os))
//        buf.write('[')
//        for (i <- Range(0, 1 * 1000 * 1000)) {
//          if (i != 0) {
//            buf.write(", ")
//          }
//          buf.write("true, false")
//        }
//        os.write(']')
//        os.flush()
//        os.close()
//
//        val arr = upickle.default.read[Array[Boolean]](Files.newInputStream(tmp))
//        (Files.size(tmp), arr.count(_ == true), arr.count(_ == false))
//      }finally{
//        Files.deleteIfExists(tmp)
//      }
//    }
  }
}

case class Big63(_0: Byte, _1: Byte, _2: Byte, _3: Byte, _4: Byte, _5: Byte, _6: Byte, _7: Byte,
                 _8: Byte, _9: Byte, _10: Byte, _11: Byte, _12: Byte, _13: Byte, _14: Byte,
                 _15: Byte, _16: Byte, _17: Byte, _18: Byte, _19: Byte, _20: Byte, _21: Byte,
                 _22: Byte, _23: Byte, _24: Byte, _25: Byte, _26: Byte, _27: Byte, _28: Byte,
                 _29: Byte, _30: Byte, _31: Byte, _32: Byte, _33: Byte, _34: Byte, _35: Byte,
                 _36: Byte, _37: Byte, _38: Byte, _39: Byte, _40: Byte, _41: Byte, _42: Byte,
                 _43: Byte, _44: Byte, _45: Byte, _46: Byte, _47: Byte, _48: Byte, _49: Byte,
                 _50: Byte, _51: Byte, _52: Byte, _53: Byte, _54: Byte, _55: Byte, _56: Byte,
                 _57: Byte, _58: Byte, _59: Byte, _60: Byte, _61: Byte, _62: Byte)
object Big63{
  implicit val b63rw: upickle.default.ReadWriter[Big63] = upickle.default.macroRW

}
case class Big64(_0: Byte, _1: Byte, _2: Byte, _3: Byte, _4: Byte, _5: Byte, _6: Byte, _7: Byte,
                 _8: Byte, _9: Byte, _10: Byte, _11: Byte, _12: Byte, _13: Byte, _14: Byte,
                 _15: Byte, _16: Byte, _17: Byte, _18: Byte, _19: Byte, _20: Byte, _21: Byte,
                 _22: Byte, _23: Byte, _24: Byte, _25: Byte, _26: Byte, _27: Byte, _28: Byte,
                 _29: Byte, _30: Byte, _31: Byte, _32: Byte, _33: Byte, _34: Byte, _35: Byte,
                 _36: Byte, _37: Byte, _38: Byte, _39: Byte, _40: Byte, _41: Byte, _42: Byte,
                 _43: Byte, _44: Byte, _45: Byte, _46: Byte, _47: Byte, _48: Byte, _49: Byte,
                 _50: Byte, _51: Byte, _52: Byte, _53: Byte, _54: Byte, _55: Byte, _56: Byte,
                 _57: Byte, _58: Byte, _59: Byte, _60: Byte, _61: Byte, _62: Byte, _63: Byte)
object Big64{
  implicit val b64rw: upickle.default.ReadWriter[Big64] = upickle.default.macroRW
}
case class Big65(_0: Byte, _1: Byte, _2: Byte, _3: Byte, _4: Byte, _5: Byte, _6: Byte, _7: Byte,
                 _8: Byte, _9: Byte, _10: Byte, _11: Byte, _12: Byte, _13: Byte, _14: Byte,
                 _15: Byte, _16: Byte, _17: Byte, _18: Byte, _19: Byte, _20: Byte, _21: Byte,
                 _22: Byte, _23: Byte, _24: Byte, _25: Byte, _26: Byte, _27: Byte, _28: Byte,
                 _29: Byte, _30: Byte, _31: Byte, _32: Byte, _33: Byte, _34: Byte, _35: Byte,
                 _36: Byte, _37: Byte, _38: Byte, _39: Byte, _40: Byte, _41: Byte, _42: Byte,
                 _43: Byte, _44: Byte, _45: Byte, _46: Byte, _47: Byte, _48: Byte, _49: Byte,
                 _50: Byte, _51: Byte, _52: Byte, _53: Byte, _54: Byte, _55: Byte, _56: Byte,
                 _57: Byte, _58: Byte, _59: Byte, _60: Byte, _61: Byte, _62: Byte, _63: Byte,
                 _64: Byte)
object Big65{
  implicit val b65rw: upickle.default.ReadWriter[Big65] = upickle.default.macroRW
}
case class Big150(_0: Int, _1: Int, _2: Int, _3: Int, _4: Int, _5: Int, _6: Int, _7: Int,
                  _8: Int, _9: Int, _10: Int, _11: Int, _12: Int, _13: Int, _14: Int,
                  _15: Int, _16: Int, _17: Int, _18: Int, _19: Int, _20: Int, _21: Int,
                  _22: Int, _23: Int, _24: Int, _25: Int, _26: Int, _27: Int, _28: Int,
                  _29: Int, _30: Int, _31: Int, _32: Int, _33: Int, _34: Int, _35: Int,
                  _36: Int, _37: Int, _38: Int, _39: Int, _40: Int, _41: Int, _42: Int,
                  _43: Int, _44: Int, _45: Int, _46: Int, _47: Int, _48: Int, _49: Int,
                  _50: Int, _51: Int, _52: Int, _53: Int, _54: Int, _55: Int, _56: Int,
                  _57: Int, _58: Int, _59: Int, _60: Int, _61: Int, _62: Int, _63: Int,
                  _64: Int, _65: Int, _66: Int, _67: Int, _68: Int, _69: Int, _70: Int,
                  _71: Int, _72: Int, _73: Int, _74: Int, _75: Int, _76: Int, _77: Int,
                  _78: Int, _79: Int, _80: Int, _81: Int, _82: Int, _83: Int, _84: Int,
                  _85: Int, _86: Int, _87: Int, _88: Int, _89: Int, _90: Int, _91: Int,
                  _92: Int, _93: Int, _94: Int, _95: Int, _96: Int, _97: Int, _98: Int,
                  _99: Int, _100: Int, _101: Int, _102: Int, _103: Int, _104: Int,
                  _105: Int, _106: Int, _107: Int, _108: Int, _109: Int, _110: Int,
                  _111: Int, _112: Int, _113: Int, _114: Int, _115: Int, _116: Int,
                  _117: Int, _118: Int, _119: Int, _120: Int, _121: Int, _122: Int,
                  _123: Int, _124: Int, _125: Int, _126: Int, _127: Int, _128: Int,
                  _129: Int, _130: Int, _131: Int, _132: Int, _133: Int, _134: Int,
                  _135: Int, _136: Int, _137: Int, _138: Int, _139: Int, _140: Int,
                  _141: Int, _142: Int, _143: Int, _144: Int, _145: Int, _146: Int,
                  _147: Int, _148: Int, _149: Int)
object Big150{

  implicit val b150rw: upickle.default.ReadWriter[Big150] = upickle.default.macroRW
}

