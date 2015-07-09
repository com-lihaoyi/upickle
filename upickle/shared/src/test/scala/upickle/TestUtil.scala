package upickle
import utest._
import upickle.default._
import acyclic.file
/**
* Created by haoyi on 4/22/14.
*/
object TestUtil{
  implicit class -->[T](x: T){
    def -->(y: T) = {
      val lhs = x
      val rhs = y
      assert(lhs == rhs)
    }
  }
  def rw[T: Reader: Writer](t: T, s: String*) = {
    rwk[T, T](t, s:_*)
  }
  def rwk[T: Reader: Writer, V](t: T, sIn: String*) = {
    val writtenT = write(t)

    val strings = sIn.map(_.trim)

    if (strings.length > 0)
      assert(strings.map(upickle.json.read).contains(upickle.json.read(writtenT)))

    for (s <- strings) {
      val readS = read[T](s)
      assert(readS == t)
    }

    assert(read[T](writtenT) == t)
  }
}
