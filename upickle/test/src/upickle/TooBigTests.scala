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
              .replace("\"__2\":2,", "")
              .replace("\"__52\":52,", "")
              .replace("\"__102\":102,", "")
              .replace("\"__142\":142,", "")
          )
        }
        assert(
          err.getMessage
            .startsWith("missing keys in dictionary: __2, __52, __102, __142 at index")
        )

        val readWithDefault1 = upickle.default.read[Big150](
          written150.replace(",\"__149\":149", "")
        )
        assert(readWithDefault1 == b150.copy(__149 = -1337))

        val readWithDefault2 = upickle.default.read[Big150](
          written150.replace("\"__0\":0,", "")
        )
        assert(readWithDefault2 == b150.copy(__0 = 31337))

        val readWithDefault3 = upickle.default.read[Big150](
          written150.replace("\"__0\":0,", "").replace(",\"__149\":149", "")
        )
        assert(readWithDefault3 == b150.copy(__0 = 31337, __149 = -1337))
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

case class Big63(__0: Byte, __1: Byte, __2: Byte, __3: Byte, __4: Byte, __5: Byte, __6: Byte, __7: Byte,
                 __8: Byte, __9: Byte, __10: Byte, __11: Byte, __12: Byte, __13: Byte, __14: Byte,
                 __15: Byte, __16: Byte, __17: Byte, __18: Byte, __19: Byte, __20: Byte, __21: Byte,
                 __22: Byte, __23: Byte, __24: Byte, __25: Byte, __26: Byte, __27: Byte, __28: Byte,
                 __29: Byte, __30: Byte, __31: Byte, __32: Byte, __33: Byte, __34: Byte, __35: Byte,
                 __36: Byte, __37: Byte, __38: Byte, __39: Byte, __40: Byte, __41: Byte, __42: Byte,
                 __43: Byte, __44: Byte, __45: Byte, __46: Byte, __47: Byte, __48: Byte, __49: Byte,
                 __50: Byte, __51: Byte, __52: Byte, __53: Byte, __54: Byte, __55: Byte, __56: Byte,
                 __57: Byte, __58: Byte, __59: Byte, __60: Byte, __61: Byte, __62: Byte)
object Big63{
  implicit val b63rw: upickle.default.ReadWriter[Big63] = upickle.default.macroRW

}
case class Big64(__0: Byte, __1: Byte, __2: Byte, __3: Byte, __4: Byte, __5: Byte, __6: Byte, __7: Byte,
                 __8: Byte, __9: Byte, __10: Byte, __11: Byte, __12: Byte, __13: Byte, __14: Byte,
                 __15: Byte, __16: Byte, __17: Byte, __18: Byte, __19: Byte, __20: Byte, __21: Byte,
                 __22: Byte, __23: Byte, __24: Byte, __25: Byte, __26: Byte, __27: Byte, __28: Byte,
                 __29: Byte, __30: Byte, __31: Byte, __32: Byte, __33: Byte, __34: Byte, __35: Byte,
                 __36: Byte, __37: Byte, __38: Byte, __39: Byte, __40: Byte, __41: Byte, __42: Byte,
                 __43: Byte, __44: Byte, __45: Byte, __46: Byte, __47: Byte, __48: Byte, __49: Byte,
                 __50: Byte, __51: Byte, __52: Byte, __53: Byte, __54: Byte, __55: Byte, __56: Byte,
                 __57: Byte, __58: Byte, __59: Byte, __60: Byte, __61: Byte, __62: Byte, __63: Byte)
object Big64{
  implicit val b64rw: upickle.default.ReadWriter[Big64] = upickle.default.macroRW
}
case class Big65(__0: Byte, __1: Byte, __2: Byte, __3: Byte, __4: Byte, __5: Byte, __6: Byte, __7: Byte,
                 __8: Byte, __9: Byte, __10: Byte, __11: Byte, __12: Byte, __13: Byte, __14: Byte,
                 __15: Byte, __16: Byte, __17: Byte, __18: Byte, __19: Byte, __20: Byte, __21: Byte,
                 __22: Byte, __23: Byte, __24: Byte, __25: Byte, __26: Byte, __27: Byte, __28: Byte,
                 __29: Byte, __30: Byte, __31: Byte, __32: Byte, __33: Byte, __34: Byte, __35: Byte,
                 __36: Byte, __37: Byte, __38: Byte, __39: Byte, __40: Byte, __41: Byte, __42: Byte,
                 __43: Byte, __44: Byte, __45: Byte, __46: Byte, __47: Byte, __48: Byte, __49: Byte,
                 __50: Byte, __51: Byte, __52: Byte, __53: Byte, __54: Byte, __55: Byte, __56: Byte,
                 __57: Byte, __58: Byte, __59: Byte, __60: Byte, __61: Byte, __62: Byte, __63: Byte,
                 __64: Byte)
object Big65{
  implicit val b65rw: upickle.default.ReadWriter[Big65] = upickle.default.macroRW
}
case class Big150(__0: Int = 31337, __1: Int, __2: Int, __3: Int, __4: Int, __5: Int, __6: Int, __7: Int,
                  __8: Int, __9: Int, __10: Int, __11: Int, __12: Int, __13: Int, __14: Int,
                  __15: Int, __16: Int, __17: Int, __18: Int, __19: Int, __20: Int, __21: Int,
                  __22: Int, __23: Int, __24: Int, __25: Int, __26: Int, __27: Int, __28: Int,
                  __29: Int, __30: Int, __31: Int, __32: Int, __33: Int, __34: Int, __35: Int,
                  __36: Int, __37: Int, __38: Int, __39: Int, __40: Int, __41: Int, __42: Int,
                  __43: Int, __44: Int, __45: Int, __46: Int, __47: Int, __48: Int, __49: Int,
                  __50: Int, __51: Int, __52: Int, __53: Int, __54: Int, __55: Int, __56: Int,
                  __57: Int, __58: Int, __59: Int, __60: Int, __61: Int, __62: Int, __63: Int,
                  __64: Int, __65: Int, __66: Int, __67: Int, __68: Int, __69: Int, __70: Int,
                  __71: Int, __72: Int, __73: Int, __74: Int, __75: Int, __76: Int, __77: Int,
                  __78: Int, __79: Int, __80: Int, __81: Int, __82: Int, __83: Int, __84: Int,
                  __85: Int, __86: Int, __87: Int, __88: Int, __89: Int, __90: Int, __91: Int,
                  __92: Int, __93: Int, __94: Int, __95: Int, __96: Int, __97: Int, __98: Int,
                  __99: Int, __100: Int, __101: Int, __102: Int, __103: Int, __104: Int,
                  __105: Int, __106: Int, __107: Int, __108: Int, __109: Int, __110: Int,
                  __111: Int, __112: Int, __113: Int, __114: Int, __115: Int, __116: Int,
                  __117: Int, __118: Int, __119: Int, __120: Int, __121: Int, __122: Int,
                  __123: Int, __124: Int, __125: Int, __126: Int, __127: Int, __128: Int,
                  __129: Int, __130: Int, __131: Int, __132: Int, __133: Int, __134: Int,
                  __135: Int, __136: Int, __137: Int, __138: Int, __139: Int, __140: Int,
                  __141: Int, __142: Int, __143: Int, __144: Int, __145: Int, __146: Int,
                  __147: Int, __148: Int, __149: Int = -1337 /*default*/){

  // Workaround for https://github.com/scala/scala/pull/9635
  override def equals(other: Any): Boolean = other match{
    case o: Big150 =>
      (__0 == o.__0 && __1 == o.__1 && __2 == o.__2 && __3 == o.__3 && __4 == o.__4 && __5 == o.__5 && __6 == o.__6 && __7 == o.__7) &&
      (__8 == o.__8 && __9 == o.__9 && __10 == o.__10 && __11 == o.__11 && __12 == o.__12 && __13 == o.__13 && __14 == o.__14) &&
      (__15 == o.__15 && __16 == o.__16 && __17 == o.__17 && __18 == o.__18 && __19 == o.__19 && __20 == o.__20 && __21 == o.__21) &&
      (__22 == o.__22 && __23 == o.__23 && __24 == o.__24 && __25 == o.__25 && __26 == o.__26 && __27 == o.__27 && __28 == o.__28) &&
      (__29 == o.__29 && __30 == o.__30 && __31 == o.__31 && __32 == o.__32 && __33 == o.__33 && __34 == o.__34 && __35 == o.__35) &&
      (__36 == o.__36 && __37 == o.__37 && __38 == o.__38 && __39 == o.__39 && __40 == o.__40 && __41 == o.__41 && __42 == o.__42) &&
      (__43 == o.__43 && __44 == o.__44 && __45 == o.__45 && __46 == o.__46 && __47 == o.__47 && __48 == o.__48 && __49 == o.__49) &&
      (__50 == o.__50 && __51 == o.__51 && __52 == o.__52 && __53 == o.__53 && __54 == o.__54 && __55 == o.__55 && __56 == o.__56) &&
      (__57 == o.__57 && __58 == o.__58 && __59 == o.__59 && __60 == o.__60 && __61 == o.__61 && __62 == o.__62 && __63 == o.__63) &&
      (__64 == o.__64 && __65 == o.__65 && __66 == o.__66 && __67 == o.__67 && __68 == o.__68 && __69 == o.__69 && __70 == o.__70) &&
      (__71 == o.__71 && __72 == o.__72 && __73 == o.__73 && __74 == o.__74 && __75 == o.__75 && __76 == o.__76 && __77 == o.__77) &&
      (__78 == o.__78 && __79 == o.__79 && __80 == o.__80 && __81 == o.__81 && __82 == o.__82 && __83 == o.__83 && __84 == o.__84) &&
      (__85 == o.__85 && __86 == o.__86 && __87 == o.__87 && __88 == o.__88 && __89 == o.__89 && __90 == o.__90 && __91 == o.__91) &&
      (__92 == o.__92 && __93 == o.__93 && __94 == o.__94 && __95 == o.__95 && __96 == o.__96 && __97 == o.__97 && __98 == o.__98) &&
      (__99 == o.__99 && __100 == o.__100 && __101 == o.__101 && __102 == o.__102 && __103 == o.__103 && __104 == o.__104) &&
      (__105 == o.__105 && __106 == o.__106 && __107 == o.__107 && __108 == o.__108 && __109 == o.__109 && __110 == o.__110) &&
      (__111 == o.__111 && __112 == o.__112 && __113 == o.__113 && __114 == o.__114 && __115 == o.__115 && __116 == o.__116) &&
      (__117 == o.__117 && __118 == o.__118 && __119 == o.__119 && __120 == o.__120 && __121 == o.__121 && __122 == o.__122) &&
      (__123 == o.__123 && __124 == o.__124 && __125 == o.__125 && __126 == o.__126 && __127 == o.__127 && __128 == o.__128) &&
      (__129 == o.__129 && __130 == o.__130 && __131 == o.__131 && __132 == o.__132 && __133 == o.__133 && __134 == o.__134) &&
      (__135 == o.__135 && __136 == o.__136 && __137 == o.__137 && __138 == o.__138 && __139 == o.__139 && __140 == o.__140) &&
      (__141 == o.__141 && __142 == o.__142 && __143 == o.__143 && __144 == o.__144 && __145 == o.__145 && __146 == o.__146) &&
      (__147 == o.__147 && __148 == o.__148 && __149 == o.__149)

    case _ => false
  }
}

object Big150{
  implicit val b150rw: upickle.default.ReadWriter[Big150] = upickle.default.macroRW
}

