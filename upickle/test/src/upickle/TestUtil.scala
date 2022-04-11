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
class TestUtil[Api <: upickle.Api](val api: Api){

  def rw[T](t: T, s: String*)
           (implicit r: api.Reader[T], w: api.Writer[T], rw: api.ReadWriter[T]) = {
    rwk[T, T](t, s:_*)(x => x)(r, w, rw)
  }

  def rwNoBinaryJson[T](t: T, s: String*)
                       (implicit r: api.Reader[T], w: api.Writer[T], rw: api.ReadWriter[T])= {
    rwk[T, T](t, s:_*)(x => x, checkBinaryJson = false)(r, w, rw)
  }

  def rwEscape[T](t: T, s: String*)
                 (implicit r: api.Reader[T], w: api.Writer[T], rw: api.ReadWriter[T])= {
    rwk[T, T](t, s:_*)(x => x, escapeUnicode = true)(r, w, rw)
  }

  def rwk[T, V](t: T, sIn: String*)
               (normalize: T => V,
                escapeUnicode: Boolean = false,
                checkBinaryJson: Boolean = true)
               (implicit r: api.Reader[T], w: api.Writer[T], rw: api.ReadWriter[T])= {
    rwk0(t, sIn:_*)(normalize, escapeUnicode, checkBinaryJson)(r, w)
    rwk0(t, sIn:_*)(normalize, escapeUnicode, checkBinaryJson)(rw, rw)
  }

  def rwk0[T, V](t: T, sIn: String*)
                (normalize: T => V,
                 escapeUnicode: Boolean = false,
                 checkBinaryJson: Boolean = true)
                (implicit r: api.Reader[T], w: api.Writer[T])= {
    val writtenT = api.write(t)
    val writtenBytesT = api.writeToByteArray(t)

    // Test JSON round tripping
    val strings = sIn.map(_.trim)

    for (s <- strings) {
      val readS = api.read[T](s)
      val normalizedReadString = normalize(readS)
      val normalizedValue = normalize(t)
      utest.assert(normalizedReadString == normalizedValue)
    }


    val normalizedReadWrittenT = normalize(api.read[T](writtenT))
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
    if (strings.nonEmpty) utest.assert(ujson.reformat(strings.head) == writtenT)
    utest.assert(normalizedReadWrittenT == normalizedT)
    utest.assert(normalizedReadByteArrayWrittenT == normalizedT)
    utest.assert(normalizedReadStreamWrittenT == normalizedT)
    utest.assert(normalizedReadSmallStreamWrittenT == normalizedT)
    utest.assert(normalizedReadByteArrayWrittenBytesT == normalizedT)
    utest.assert(normalizedReadStreamWrittenBytesT == normalizedT)
    utest.assert(normalizedReadSmallStreamWrittenBytesT == normalizedT)


    // Test MessagePack round tripping
    val writtenBinary = api.writeBinary(t)
    // println(upickle.core.Util.bytesToString(writtenBinary))
    val roundTrippedBinary = api.readBinary[T](writtenBinary)
    (roundTrippedBinary, t) match{
      case (lhs: Array[_], rhs: Array[_]) => assert(lhs.toSeq == rhs.toSeq)
      case _ => utest.assert(roundTrippedBinary == t)
    }

    // Test binary-JSON equivalence
    if (checkBinaryJson){
      val rewrittenBinary = api.writeBinary(roundTrippedBinary)

      val writtenBinaryStr = upickle.core.Util.bytesToString(writtenBinary)
      val rewrittenBinaryStr = upickle.core.Util.bytesToString(rewrittenBinary)
      utest.assert(writtenBinaryStr == rewrittenBinaryStr)
    }
  }

  def rwNum[T: Numeric: api.Reader: api.Writer](t: T, strings: String*) = {
    rw(t, strings: _*)
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