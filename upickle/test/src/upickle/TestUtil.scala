package upickle
import utest._
import acyclic.file
/**
* Created by haoyi on 4/22/14.
*/
object TestUtil extends TestUtil[upickle.default.type](upickle.default)
object LegacyTestUtil extends TestUtil[upickle.legacy.type](upickle.legacy)
class TestUtil[Api <: upickle.Api](val api: Api){

  def rw[T: api.Reader: api.Writer](t: T, s: String*) = {
    rwk[T, T](t, s:_*)(x => x)
  }
  def rwEscape[T: api.Reader: api.Writer](t: T, s: String*) = {
    rwk[T, T](t, s:_*)(x => x, escapeUnicode = true)
  }
  def rwk[T: api.Reader: api.Writer, V](t: T, sIn: String*)(normalize: T => V, escapeUnicode: Boolean = false) = {
    val writtenT = api.write(t)

    // Test JSON round tripping
    val strings = sIn.map(_.trim)

    for (s <- strings) {
      val readS = api.read[T](s)
      val normalizedReadString = normalize(readS)
      val normalizedValue = normalize(t)
      assert(normalizedReadString == normalizedValue)
    }

    val normalizedReadWrittenT = normalize(api.read[T](writtenT))
    val normalizedT = normalize(t)
    assert(normalizedReadWrittenT == normalizedT)

    // Test binary round tripping
    val writtenBinary = api.writeBinary(t)
    val roundTrippedBinary = api.readBinary[T](writtenBinary)
    (roundTrippedBinary, t) match{
      case (lhs: Array[_], rhs: Array[_]) => assert(lhs.toSeq == rhs.toSeq)
      case _ => assert(roundTrippedBinary == t)
    }


    val rewrittenBinary = api.writeBinary(roundTrippedBinary)

    val writtenBinaryStr = upickle.core.Util.bytesToString(writtenBinary)
    val rewrittenBinaryStr = upickle.core.Util.bytesToString(rewrittenBinary)
    assert(writtenBinaryStr == rewrittenBinaryStr)

    val jsonifiedWrittenBinary = upack
      .transform(rewrittenBinary, new ujson.StringRenderer())
      .toString
    val jsonifiedRewrittenBinary = upack
      .transform(rewrittenBinary, new ujson.StringRenderer())
      .toString

    // Test binary conversion
    assert(
      jsonifiedWrittenBinary == jsonifiedRewrittenBinary,
      jsonifiedWrittenBinary == writtenT
    )
  }
}