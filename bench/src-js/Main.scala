package upickle

import upickle.ADTs.ADT0
import upickle.Common.{Data, bench}
import upickle.Defaults.ADTc
import upickle.Hierarchy.{A, B, C}
import upickle.Recursive.{End, LL, Node}
import scala.scalajs.js
object Main{
  def main(args: Array[String]): Unit = {
    for(duration <- Seq(2500, 5000, 10000, 10000, 10000)){
      println("RUN JS: " + duration)
      println()
//      rawJsonParseSerialize(duration)
      NonNative.playJson(duration)
      NonNative.circe(duration)
      Common.upickleDefault(duration)
      Common.upickleDefaultByteArray(duration)
      Common.upickleBinaryDefault(duration)
      upickleWebDefault(duration)
      //      Common.upickleLegacy(duration)
//      Common.upickleBinaryLegacy(duration)
//      Common.genCodec(duration)
//      upickleWebDefault(duration)
//      upickleWebLegacy(duration)
      NonNative.playJsonCached(duration)
      NonNative.circeCached(duration)
      upickleWebDefaultCached(duration)
      Common.upickleDefaultCached(duration)
      Common.upickleDefaultCachedByteArray(duration)
      Common.upickleDefaultBinaryCached(duration)
      //      Common.upickleLegacyCached(duration)
      //      Common.upickleLegacyBinaryCached(duration)
      //      Common.genCodecCached(duration)
//      upickleWebLegacyCached(duration)
      println()
    }
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
