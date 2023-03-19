package upickle

import upickle.ADTs.ADT0
import upickle.Common.{Data, bench}
import upickle.Defaults.ADTc
import upickle.Hierarchy.{A, B, C}
import upickle.Recursive.{End, LL, Node}
import scala.scalajs.js
object Main{
  def main(args: Array[String]): Unit = {
    val allResults = collection.mutable.Buffer.empty[(String, Int)]
    for((duration, save) <- Seq(2500 -> false, 5000 -> false, 10000 -> true, 10000 -> true, 10000 -> true)){
      println("RUN JS: " + duration)
      println()
//      rawJsonParseSerialize(duration)
//      NonNative.playJson(duration)
//      NonNative.circe(duration)
//      Common.upickleDefault(duration)
//      Common.upickleDefaultByteArray(duration)
//      Common.upickleDefaultBinary(duration)
//      upickleWebDefault(duration)
      //      Common.upickleLegacy(duration)
//      Common.upickleBinaryLegacy(duration)
//      Common.genCodec(duration)
//      upickleWebDefault(duration)
//      upickleWebLegacy(duration)
//      NonNative.playJsonCached(duration)
//      NonNative.circeCached(duration)
//      upickleWebDefaultCached(duration)
//      Common.upickleDefaultCached(duration)
//      Common.upickleDefaultByteArrayCached(duration)
//      Common.upickleDefaultBinaryCached(duration)
      //      Common.upickleLegacyCached(duration)
      //      Common.upickleLegacyBinaryCached(duration)
      //      Common.genCodecCached(duration)
//      upickleWebLegacyCached(duration)
      val results = Seq(
        Common.upickleDefault(duration),
        Common.upickleDefaultByteArray(duration),
        Common.upickleDefaultBinary(duration),
        Common.integers(duration),
        Common.integersByteArray(duration),
        Common.integersBinary(duration),

        Common.doubles(duration),
        Common.doublesByteArray(duration),
        Common.doublesBinary(duration),

        Common.sequences(duration),
        Common.sequencesByteArray(duration),
        Common.sequencesBinary(duration),

        Common.shortStrings(duration),
        Common.shortStringsByteArray(duration),
        Common.shortStringsBinary(duration),

        Common.longStrings(duration),
        Common.longStringsByteArray(duration),
        Common.longStringsBinary(duration),

        Common.unicodeStrings(duration),
        Common.unicodeStringsByteArray(duration),
        Common.unicodeStringsBinary(duration),

        Common.caseClasses(duration),
        Common.caseClassesByteArray(duration),
        Common.caseClassesBinary(duration)
      )
      println()
      if (save) allResults.appendAll(results.flatten)
      println()
    }
    Common.prettyPrintResults(allResults)
  }

  def rawJsonParseSerialize(duration: Int) = {

    Common.bench0[String, js.Any](duration, Common.benchmarkSampleJson)(
      js.JSON.parse(_),
      js.JSON.stringify(_)
    )
  }
  def upickleWebDefault(duration: Int) = {
    import upickle.default.{ReadWriter => RW}
    implicit def rw1: RW[Data] = upickle.default.macroRW
    implicit def rw2: RW[A] = upickle.default.macroRW
    implicit def rw3: RW[B] = upickle.default.macroRW
    implicit def rw4: RW[C] = upickle.default.macroRW
    implicit def rw5: RW[LL] = upickle.default.macroRW
    implicit def rw6: RW[Node] = upickle.default.macroRW
    implicit def rw7: RW[End.type] = upickle.default.macroRW
    implicit def rw8: RW[ADTc] = upickle.default.macroRW
    implicit def rw9: RW[ADT0] = upickle.default.macroRW
    bench[String](duration)(
      upickle.default.web.read[Seq[Data]],
      upickle.default.web.write(_)
    )
  }
  def upickleWebDefaultCached(duration: Int) = {
    import upickle.default.{ReadWriter => RW}
    implicit lazy val rw1: RW[Data] = upickle.default.macroRW
    implicit lazy val rw2: RW[A] = upickle.default.macroRW
    implicit lazy val rw3: RW[B] = upickle.default.macroRW
    implicit lazy val rw4: RW[C] = upickle.default.macroRW
    implicit lazy val rw5: RW[LL] = upickle.default.macroRW
    implicit lazy val rw6: RW[Node] = upickle.default.macroRW
    implicit lazy val rw7: RW[End.type] = upickle.default.macroRW
    implicit lazy val rw8: RW[ADTc] = upickle.default.macroRW
    implicit lazy val rw9: RW[ADT0] = upickle.default.macroRW
    bench[String](duration)(
      upickle.default.web.read[Seq[Data]],
      upickle.default.web.write(_)
    )
  }
  def upickleWebLegacy(duration: Int) = {
    import upickle.legacy.{ReadWriter => RW}
    implicit def rw1: RW[Data] = upickle.legacy.macroRW
    implicit def rw2: RW[A] = upickle.legacy.macroRW
    implicit def rw3: RW[B] = upickle.legacy.macroRW
    implicit def rw4: RW[C] = upickle.legacy.macroRW
    implicit def rw5: RW[LL] = upickle.legacy.macroRW
    implicit def rw6: RW[Node] = upickle.legacy.macroRW
    implicit def rw7: RW[End.type] = upickle.legacy.macroRW
    implicit def rw8: RW[ADTc] = upickle.legacy.macroRW
    implicit def rw9: RW[ADT0] = upickle.legacy.macroRW
    bench[String](duration)(
      upickle.legacy.web.read[Seq[Data]],
      upickle.legacy.web.write(_)
    )
  }
  def upickleWebLegacyCached(duration: Int) = {
    import upickle.legacy.{ReadWriter => RW}
    implicit lazy val rw1: RW[Data] = upickle.legacy.macroRW
    implicit lazy val rw2: RW[A] = upickle.legacy.macroRW
    implicit lazy val rw3: RW[B] = upickle.legacy.macroRW
    implicit lazy val rw4: RW[C] = upickle.legacy.macroRW
    implicit lazy val rw5: RW[LL] = upickle.legacy.macroRW
    implicit lazy val rw6: RW[Node] = upickle.legacy.macroRW
    implicit lazy val rw7: RW[End.type] = upickle.legacy.macroRW
    implicit lazy val rw8: RW[ADTc] = upickle.legacy.macroRW
    implicit lazy val rw9: RW[ADT0] = upickle.legacy.macroRW
    bench[String](duration)(
      upickle.legacy.web.read[Seq[Data]],
      upickle.legacy.web.write(_)
    )
  }
}
