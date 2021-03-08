package upack

import java.io.ByteArrayInputStream
import java.nio.ByteBuffer

import upickle.core.NoOpVisitor

import scala.util.Try

object TestUtil {
  def checkParse(s: Array[Byte], success: Boolean): Unit = {
    val r1a = Try(upack.read(new ByteArrayInputStream(s)))
    val r2a = Try(upack.read(s))
    val r3a = Try(new InputStreamMsgPackReader(new ByteArrayInputStream(s), 2, 2).parse(upack.Msg))

    val r1b = Try(upack.validate(new ByteArrayInputStream(s)))
    val r2b = Try(upack.validate(s))
    val r3b = Try(new InputStreamMsgPackReader(new ByteArrayInputStream(s), 2, 2).parse(NoOpVisitor))

    if (success){
      if (r1a != r2a) sys.error(s"Stream/ByteArray parsing disagree($r1a, $r2a): $s")
      if (r2a != r3a) sys.error(s"ByteArray/Small-Buffer Stream parsing disagree($r1a, $r2a): $s")

      if (r1b != r2b) sys.error(s"Stream/ByteArray parsing disagree($r1b, $r2b): $s")
      if (r2b != r3b) sys.error(s"ByteArray/Small-Buffer Stream parsing disagree($r1b, $r2b): $s")
    }else{
      assert(r1a.isFailure)
      assert(r2a.isFailure)
      assert(r3a.isFailure)

      assert(r1b.isFailure)
      assert(r2b.isFailure)
      assert(r3b.isFailure)
    }
  }
}
