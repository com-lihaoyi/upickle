package ujson

import java.io.ByteArrayInputStream
import java.nio.ByteBuffer

import scala.util.Try

object TestUtil {
  def checkParse(s: String, success: Boolean): Unit = {
    val cs = java.nio.CharBuffer.wrap(s.toCharArray)
    val bb = ByteBuffer.wrap(s.getBytes("UTF-8"))

    val r0a = Try(ujson.read(cs))
    val r1a = Try(ujson.read(s))
    val r2a = Try(ujson.read(bb))
    val r3a = Try(ujson.read(new ByteArrayInputStream(s.getBytes)))
    val r4a = Try(ujson.read(s.getBytes))

    val r0b = Try(ujson.reformat(cs))
    val r1b = Try(ujson.reformat(s))
    val r2b = Try(ujson.reformat(bb))
    val r3b = Try(ujson.reformat(new ByteArrayInputStream(s.getBytes)))
    val r4b = Try(ujson.reformat(s.getBytes))

    val r0c = Try(ujson.validate(cs))
    val r1c = Try(ujson.validate(s))
    val r2c = Try(ujson.validate(bb))
    val r3c = Try(ujson.validate(new ByteArrayInputStream(s.getBytes)))
    val r4c = Try(ujson.validate(s.getBytes))

    val r0d = Try(ujson.reformatToByteArray(cs))
    val r1d = Try(ujson.reformatToByteArray(s))
    val r2d = Try(ujson.reformatToByteArray(bb))
    val r3d = Try(ujson.reformatToByteArray(new ByteArrayInputStream(s.getBytes)))
    val r4d = Try(ujson.reformatToByteArray(s.getBytes))

    if (success){
      if (r0a != r1a) sys.error(s"CharSequence/String parsing disagree($r0a, $r1a): $s")
      if (r1a != r2a) sys.error(s"String/ByteBuffer parsing disagree($r1a, $r2a): $s")
      if (r2a != r3a) sys.error(s"ByteBuffer/InputStream parsing disagree($r2a, $r3a): $s")
      if (r3a != r4a) sys.error(s"InputStream/ByteArray parsing disagree($r3a, $r4a): $s")

      if (r0b != r1b) sys.error(s"CharSequence/String parsing disagree($r0b, $r1b): $s")
      if (r1b != r2b) sys.error(s"String/ByteBuffer parsing disagree($r1b, $r2b): $s")
      if (r2b != r3b) sys.error(s"ByteBuffer/InputStream parsing disagree($r2b, $r3b): $s")
      if (r3b != r4b) sys.error(s"InputStream/ByteArray parsing disagree($r3b, $r4b): $s")

      if (r0c != r1c) sys.error(s"CharSequence/String parsing disagree($r0c, $r1c): $s")
      if (r1c != r2c) sys.error(s"String/ByteBuffer parsing disagree($r1c, $r2c): $s")
      if (r2c != r3c) sys.error(s"ByteBuffer/InputStream parsing disagree($r2c, $r3c): $s")
      if (r3c != r4c) sys.error(s"InputStream/ByteArray parsing disagree($r3c, $r4c): $s")

      if (r0d != r1d) sys.error(s"CharSequence/String parsing disagree($r0d, $r1d): $s")
      if (r1d != r2d) sys.error(s"String/ByteBuffer parsing disagree($r1d, $r2d): $s")
      if (r2d != r3d) sys.error(s"ByteBuffer/InputStream parsing disagree($r2d, $r3d): $s")
      if (r3d != r4d) sys.error(s"InputStream/ByteArray parsing disagree($r3d, $r4d): $s")
    }else{
      assert(r0a.isFailure)
      assert(r1a.isFailure)
      assert(r2a.isFailure)
      assert(r3a.isFailure)
      assert(r4a.isFailure)

      assert(r0b.isFailure)
      assert(r1b.isFailure)
      assert(r2b.isFailure)
      assert(r3b.isFailure)
      assert(r4b.isFailure)

      assert(r0c.isFailure)
      assert(r1c.isFailure)
      assert(r2c.isFailure)
      assert(r3c.isFailure)
      assert(r4c.isFailure)

      assert(r0d.isFailure)
      assert(r1d.isFailure)
      assert(r2d.isFailure)
      assert(r3d.isFailure)
      assert(r4d.isFailure)
    }
  }
}
