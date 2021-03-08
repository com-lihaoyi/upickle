package upack

import java.io.ByteArrayInputStream
import java.nio.ByteBuffer

import scala.util.Try

object TestUtil {
  def checkParse(s: Array[Byte], success: Boolean): Unit = {
    val r1a = Try(upack.read(new ByteArrayInputStream(s)))
    val r2a = Try(upack.read(s))

    val r1b = Try(upack.validate(new ByteArrayInputStream(s)))
    val r2b = Try(upack.validate(s))

    if (success){
      if (r1a != r2a) sys.error(s"String/Stream parsing disagree($r1a, $r2a): $s")

      if (r1b != r2b) sys.error(s"String/Stream parsing disagree($r1b, $r2b): $s")
    }else{
      assert(r1a.isFailure)
      assert(r2a.isFailure)

      assert(r1b.isFailure)
      assert(r2b.isFailure)
    }
  }
}
