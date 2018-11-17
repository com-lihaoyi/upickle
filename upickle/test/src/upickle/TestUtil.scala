package upickle
import utest._
import acyclic.file
/**
* Created by haoyi on 4/22/14.
*/
object TestUtil extends TestUtil[upickle.default.type](upickle.default)
object LegacyTestUtil extends TestUtil[upickle.legacy.type](upickle.legacy)
class TestUtil[Api <: upickle.Api](val api: Api){
  import api._
  def rw[T: Reader: Writer](t: T, s: String*) = {
    rwk[T, T](t, s:_*)(x => x)
  }
  def rwEscape[T: Reader: Writer](t: T, s: String*) = {
    rwk[T, T](t, s:_*)(x => x, escapeUnicode = true)
  }
  def rwk[T: Reader: Writer, V](t: T, sIn: String*)(normalize: T => V, escapeUnicode: Boolean = false) = {
    val writtenT = write(t, escapeUnicode = true)
    val writtenBinary = writeBinary(t)
    println(upickle.core.Util.bytesToString(writtenBinary))
    val roundTrippedBinary = readBinary[T](writtenBinary)

    val strings = sIn.map(_.trim)

//    if (strings.length > 0) {
//      val inputs = strings.map(api.read)
//      val output = api.read(writtenT)
//      assert(inputs.contains(output))
//    }
    for (s <- strings) {
      val readS = read[T](s)
      val normalizedReadString = normalize(readS)
      val normalizedValue = normalize(t)
      assert(normalizedReadString == normalizedValue)
    }

    val normalizedReadWrittenT = normalize(read[T](writtenT))
    val normalizedT = normalize(t)
    assert(normalizedReadWrittenT == normalizedT)
  }
}