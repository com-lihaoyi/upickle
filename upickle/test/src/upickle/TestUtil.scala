package upickle
// TODO: utest's assert throws a cyclic dependency error in the version currently
// published for Dotty. Use utest's assert once the fix has been published in
// a new version.
import utest.{assert => _, _}
/**
* Created by haoyi on 4/22/14.
*/
object TestUtil extends TestUtil[upickle.default.type](upickle.default)
object LegacyTestUtil extends TestUtil[upickle.legacy.type](upickle.legacy)
class TestUtil[Api <: upickle.Api](val api: Api){

  def rw[T: api.Reader: api.Writer](t: T, s: String*) = {
    rwk[T, T](t, s:_*)(x => x)
  }
  def rwNoBinaryJson[T: api.Reader: api.Writer](t: T, s: String*) = {
    rwk[T, T](t, s:_*)(x => x, checkBinaryJson = false)
  }
  def rwEscape[T: api.Reader: api.Writer](t: T, s: String*) = {
    rwk[T, T](t, s:_*)(x => x, escapeUnicode = true)
  }
  def rwk[T: api.Reader: api.Writer, V](t: T, sIn: String*)
                                       (normalize: T => V,
                                        escapeUnicode: Boolean = false,
                                        checkBinaryJson: Boolean = true) = {
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
    // println(upickle.core.Util.bytesToString(writtenBinary))
    val roundTrippedBinary = api.readBinary[T](writtenBinary)
    (roundTrippedBinary, t) match{
      case (lhs: Array[_], rhs: Array[_]) => assert(lhs.toSeq == rhs.toSeq)
      case _ => assert(roundTrippedBinary == t)
    }


    // Test binary-JSON equivalence
    if (checkBinaryJson){
      val rewrittenBinary = api.writeBinary(roundTrippedBinary)

      val writtenBinaryStr = upickle.core.Util.bytesToString(writtenBinary)
      val rewrittenBinaryStr = upickle.core.Util.bytesToString(rewrittenBinary)
      assert(writtenBinaryStr == rewrittenBinaryStr)
    }
  }
}
