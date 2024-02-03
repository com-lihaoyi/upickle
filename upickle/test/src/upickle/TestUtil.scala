package upickle
// TODO: utest's assert throws a cyclic dependency error in the version currently
// published for Dotty. Use utest's assert once the fix has been published in
// a new version.
import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import scala.Numeric.Implicits._

import utest.{assert => _, _}
/**
 * Created by haoyi on 4/22/14.
 */
object TestUtil extends TestUtil[upickle.default.type](upickle.default)
object LegacyTestUtil extends TestUtil[upickle.legacy.type](upickle.legacy)
sealed trait TestValue
object TestValue{
  implicit class Json(val value: String) extends TestValue
  implicit class Msg(val value: upack.Msg) extends TestValue
}
class TestUtil[Api <: upickle.Api](val api: Api){

  /**
   * Tests reading and writing of a value `t`, ensuring it can be round-tripped
   * through the various mechanisms uPickle provides to read and write things
   *
   * You can also provide one or more [[TestValue]]s that represent JSON `String`s
   * or [[upack.Msg]]s that you expect `t` to serialize to. The first `String`
   * and first [[upack.Msg]] is asserted to be both the value that `t` is written
   * to as well as a value that can be read into `t`, while subsequent `String`s
   * and [[upack.Msg]]s are only asserted that they can be read into `t`.
   *
   * If no [[TestValue]]s are provided, then we only assert that `t` can be
   * round-tripped, without any assertions on how exactly it is serialized.
   */
  def rw[T](t: T, values: TestValue*)
           (implicit r: api.Reader[T], w: api.Writer[T], rw: api.ReadWriter[T]) = {
    rwk[T, T](t, values:_*)(x => x)(r, w, rw)
  }

  def rwNoBinaryJson[T](t: T, values: TestValue*)
                       (implicit r: api.Reader[T], w: api.Writer[T], rw: api.ReadWriter[T])= {
    rwk[T, T](t, values:_*)(x => x, checkBinaryJson = false)(r, w, rw)
  }

  def rwEscape[T](t: T, values: TestValue*)
                 (implicit r: api.Reader[T], w: api.Writer[T], rw: api.ReadWriter[T])= {
    rwk[T, T](t, values:_*)(x => x, escapeUnicode = true)(r, w, rw)
  }

  def rwk[T, V](t: T, values: TestValue*)
               (normalize: T => V,
                escapeUnicode: Boolean = false,
                checkBinaryJson: Boolean = true)
               (implicit r: api.Reader[T], w: api.Writer[T], rw: api.ReadWriter[T])= {
    rwk0(t, values:_*)(normalize, escapeUnicode, checkBinaryJson)(r, w)
    rwk0(t, values:_*)(normalize, escapeUnicode, checkBinaryJson)(rw, rw)
  }

  def rwk0[T, V](t: T, values: TestValue*)
                (normalize: T => V,
                 escapeUnicode: Boolean = false,
                 checkBinaryJson: Boolean = true)
                (implicit r: api.Reader[T], w: api.Writer[T])= {
    val writtenT = api.write(t)
    val writtenTSorted = api.write(t, sortKeys = true)
    val writtenJsT = api.writeJs(t)
    val writtenTMsg = api.writeMsg(t)
    val writtenBytesT = api.writeToByteArray(t)
    val writtenBytesTMsg = api.writeBinaryToByteArray(t)
    val writtenBytesTMsgSorted = api.writeBinaryToByteArray(t, sortKeys = true)
    val writeToDest = new java.io.StringWriter
    val writeBinaryToDest = new java.io.ByteArrayOutputStream

    api.writeTo(t, writeToDest)
    api.writeBinaryTo(t, writeBinaryToDest)

    // Test JSON round tripping
    val strings = values.collect{case s: TestValue.Json => s.value.trim}
    val msgs = values.collect{case s: TestValue.Msg => s.value}

    for (s <- strings) {
      val readS = api.read[T](s)
      val normalizedReadString = normalize(readS)
      val normalizedValue = normalize(t)
      utest.assert(normalizedReadString == normalizedValue)
    }

    for (s <- msgs) {
      val readS = api.readBinary[T](s)
      val normalizedReadString = normalize(readS)
      val normalizedValue = normalize(t)
      utest.assert(normalizedReadString == normalizedValue)
    }

    val normalizedReadWrittenT = normalize(api.read[T](writtenT))
    val normalizedReadWrittenToT = normalize(api.read[T](writeToDest.toString))
    val normalizedReadWrittenTSorted = normalize(api.read[T](writtenTSorted))

    val normalizedReadWrittenJsT = normalize(api.read[T](writtenJsT))
    val normalizedReadByteArrayWrittenT = normalize(
      api.read[T](writtenT.getBytes(StandardCharsets.UTF_8))
    )
    val normalizedReadStreamWrittenT = normalize(
      api.read[T](
        new ByteArrayInputStream(writtenT.getBytes(StandardCharsets.UTF_8))
      )
    )
    val normalizedReadSmallStreamWrittenT = normalize(
      new ujson.InputStreamParser(
        new ByteArrayInputStream(writtenT.getBytes(StandardCharsets.UTF_8)),
        2, 2
      ).parse(api.reader[T])
    )
    val normalizedReadByteArrayWrittenBytesT = normalize(
      api.read[T](writtenBytesT)
    )
    val normalizedReadStreamWrittenBytesT = normalize(
      api.read[T](
        new ByteArrayInputStream(writtenBytesT)
      )
    )
    val normalizedReadSmallStreamWrittenBytesT = normalize(
      new ujson.InputStreamParser(new ByteArrayInputStream(writtenBytesT), 2, 2)
        .parse(api.reader[T])
    )
    val normalizedT = normalize(t)
    if (strings.nonEmpty) {
      utest.assert(ujson.reformat(strings.head) == writtenT)
      utest.assert(ujson.reformat(strings.head) == writtenJsT.render())
    }
    if (msgs.nonEmpty) {
      utest.assert(msgs.head == writtenTMsg)
    }
    utest.assert(normalizedReadWrittenT == normalizedT)
    utest.assert(normalizedReadWrittenToT == normalizedT)
    utest.assert(normalizedReadWrittenT == normalizedReadWrittenTSorted)
    utest.assert(normalizedReadWrittenT == normalizedReadWrittenJsT)
    utest.assert(normalizedReadByteArrayWrittenT == normalizedT)
    utest.assert(normalizedReadStreamWrittenT == normalizedT)
    utest.assert(normalizedReadSmallStreamWrittenT == normalizedT)
    utest.assert(normalizedReadByteArrayWrittenBytesT == normalizedT)
    utest.assert(normalizedReadStreamWrittenBytesT == normalizedT)
    utest.assert(normalizedReadSmallStreamWrittenBytesT == normalizedT)


    // Test MessagePack round tripping
    val writtenBinary = api.writeBinary(t)
    // println(upickle.core.Util.bytesToString(writtenBinary))
    val cases = Seq(
      writtenBinary -> false,
      writtenBytesTMsg -> false,
      writtenBytesTMsgSorted -> true,
      writeBinaryToDest.toByteArray -> false
    )
    for((b, isSorted) <- cases){
      val roundTrippedBinary = api.readBinary[T](b)
      (roundTrippedBinary, t) match{
        case (lhs: Array[_], rhs: Array[_]) => assert(lhs.toSeq == rhs.toSeq)
        case _ => utest.assert(roundTrippedBinary == t)
      }

      // Test binary-JSON equivalence
      if (!isSorted && checkBinaryJson){
        val rewrittenBinary = api.writeBinary(roundTrippedBinary)

        val writtenBinaryStr = upickle.core.ParseUtils.bytesToString(writtenBinary)
        val rewrittenBinaryStr = upickle.core.ParseUtils.bytesToString(rewrittenBinary)
        utest.assert(writtenBinaryStr == rewrittenBinaryStr)
      }
    }

  }

  def rwNum[T: Numeric: api.Reader: api.Writer](t: T, values: TestValue*) = {
    rw(t, values: _*)
    num(t)
  }

  def num[T: Numeric : api.Reader](t: T) = {
    api.reader[T].visitFloat32(t.toFloat, -1) ==> t.toFloat
    api.reader[T].visitFloat64(t.toDouble, -1) ==> t.toDouble
    api.reader[T].visitInt32(t.toInt, -1) ==> t.toInt
    api.reader[T].visitInt64(t.toLong, -1) ==> t.toLong
    api.reader[T].visitFloat64String(t.toLong.toString, -1) ==> t.toLong
  }
}