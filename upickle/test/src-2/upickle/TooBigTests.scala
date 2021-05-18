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
          140, 141, 142, 143, 144, 145, 146, 147, 148, 149
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

        val readWithDefault1 = upickle.default.read[Big150](
          written150.replace(",\"_149\":149", "")
        )
        assert(readWithDefault1 == b150.copy(_149 = -1337))

        val readWithDefault2 = upickle.default.read[Big150](
          written150.replace("\"_0\":0,", "")
        )
        assert(readWithDefault2 == b150.copy(_0 = 31337))

        val readWithDefault3 = upickle.default.read[Big150](
          written150.replace("\"_0\":0,", "").replace(",\"_149\":149", "")
        )
        assert(readWithDefault3 == b150.copy(_0 = 31337, _149 = -1337))
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
case class Big150(_0: Int = 31337, _1: Int, _2: Int, _3: Int, _4: Int, _5: Int, _6: Int, _7: Int,
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
                  _147: Int, _148: Int, _149: Int = -1337 /*default*/){

  // Workaround for https://github.com/scala/scala/pull/9635
  override def equals(other: Any): Boolean = other match{
    case o: Big150 =>
      (_0 == o._0 && _1 == o._1 && _2 == o._2 && _3 == o._3 && _4 == o._4 && _5 == o._5 && _6 == o._6 && _7 == o._7) &&
      (_8 == o._8 && _9 == o._9 && _10 == o._10 && _11 == o._11 && _12 == o._12 && _13 == o._13 && _14 == o._14) &&
      (_15 == o._15 && _16 == o._16 && _17 == o._17 && _18 == o._18 && _19 == o._19 && _20 == o._20 && _21 == o._21) &&
      (_22 == o._22 && _23 == o._23 && _24 == o._24 && _25 == o._25 && _26 == o._26 && _27 == o._27 && _28 == o._28) &&
      (_29 == o._29 && _30 == o._30 && _31 == o._31 && _32 == o._32 && _33 == o._33 && _34 == o._34 && _35 == o._35) &&
      (_36 == o._36 && _37 == o._37 && _38 == o._38 && _39 == o._39 && _40 == o._40 && _41 == o._41 && _42 == o._42) &&
      (_43 == o._43 && _44 == o._44 && _45 == o._45 && _46 == o._46 && _47 == o._47 && _48 == o._48 && _49 == o._49) &&
      (_50 == o._50 && _51 == o._51 && _52 == o._52 && _53 == o._53 && _54 == o._54 && _55 == o._55 && _56 == o._56) &&
      (_57 == o._57 && _58 == o._58 && _59 == o._59 && _60 == o._60 && _61 == o._61 && _62 == o._62 && _63 == o._63) &&
      (_64 == o._64 && _65 == o._65 && _66 == o._66 && _67 == o._67 && _68 == o._68 && _69 == o._69 && _70 == o._70) &&
      (_71 == o._71 && _72 == o._72 && _73 == o._73 && _74 == o._74 && _75 == o._75 && _76 == o._76 && _77 == o._77) &&
      (_78 == o._78 && _79 == o._79 && _80 == o._80 && _81 == o._81 && _82 == o._82 && _83 == o._83 && _84 == o._84) &&
      (_85 == o._85 && _86 == o._86 && _87 == o._87 && _88 == o._88 && _89 == o._89 && _90 == o._90 && _91 == o._91) &&
      (_92 == o._92 && _93 == o._93 && _94 == o._94 && _95 == o._95 && _96 == o._96 && _97 == o._97 && _98 == o._98) &&
      (_99 == o._99 && _100 == o._100 && _101 == o._101 && _102 == o._102 && _103 == o._103 && _104 == o._104) &&
      (_105 == o._105 && _106 == o._106 && _107 == o._107 && _108 == o._108 && _109 == o._109 && _110 == o._110) &&
      (_111 == o._111 && _112 == o._112 && _113 == o._113 && _114 == o._114 && _115 == o._115 && _116 == o._116) &&
      (_117 == o._117 && _118 == o._118 && _119 == o._119 && _120 == o._120 && _121 == o._121 && _122 == o._122) &&
      (_123 == o._123 && _124 == o._124 && _125 == o._125 && _126 == o._126 && _127 == o._127 && _128 == o._128) &&
      (_129 == o._129 && _130 == o._130 && _131 == o._131 && _132 == o._132 && _133 == o._133 && _134 == o._134) &&
      (_135 == o._135 && _136 == o._136 && _137 == o._137 && _138 == o._138 && _139 == o._139 && _140 == o._140) &&
      (_141 == o._141 && _142 == o._142 && _143 == o._143 && _144 == o._144 && _145 == o._145 && _146 == o._146) &&
      (_147 == o._147 && _148 == o._148 && _149 == o._149)

    case _ => false
  }
}

object Big150{
  implicit val b150rw: upickle.default.ReadWriter[Big150] = upickle.default.macroRW
}

