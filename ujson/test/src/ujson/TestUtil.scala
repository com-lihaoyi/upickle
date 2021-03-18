package ujson

import java.io.ByteArrayInputStream
import java.nio.ByteBuffer

import upickle.core.NoOpVisitor

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
    val r5a = Try(new InputStreamParser(new ByteArrayInputStream(s.getBytes), 2, 2).parse(ujson.Value))

    val r0b = Try(ujson.reformat(cs))
    val r1b = Try(ujson.reformat(s))
    val r2b = Try(ujson.reformat(bb))
    val r3b = Try(ujson.reformat(new ByteArrayInputStream(s.getBytes)))
    val r4b = Try(ujson.reformat(s.getBytes))
    val r5b = Try(new InputStreamParser(new ByteArrayInputStream(s.getBytes), 2, 2).parse(ujson.StringRenderer()).toString)

    val r0c = Try(ujson.validate(cs))
    val r1c = Try(ujson.validate(s))
    val r2c = Try(ujson.validate(bb))
    val r3c = Try(ujson.validate(new ByteArrayInputStream(s.getBytes)))
    val r4c = Try(ujson.validate(s.getBytes))
    val r5c = Try(new InputStreamParser(new ByteArrayInputStream(s.getBytes), 2, 2).parse(NoOpVisitor))

    val r0d = Try(ujson.reformatToByteArray(cs).toList)
    val r1d = Try(ujson.reformatToByteArray(s).toList)
    val r2d = Try(ujson.reformatToByteArray(bb).toList)
    val r3d = Try(ujson.reformatToByteArray(new ByteArrayInputStream(s.getBytes)).toList)
    val r4d = Try(ujson.reformatToByteArray(s.getBytes).toList)
    val r5d = Try(new InputStreamParser(new ByteArrayInputStream(s.getBytes), 2, 2).parse(BytesRenderer()).toByteArray.toList)

    if (success){
      if (r0a != r1a) sys.error(s"CharSequence/String parsing disagree($r0a, $r1a): $s")
      if (r1a != r2a) sys.error(s"String/ByteBuffer parsing disagree($r1a, $r2a): $s")
      if (r2a != r3a) sys.error(s"ByteBuffer/InputStream parsing disagree($r2a, $r3a): $s")
      if (r3a != r4a) sys.error(s"InputStream/ByteArray parsing disagree($r3a, $r4a): $s")
      if (r4a != r5a) sys.error(s"ByteArray/Small-Buffer InputStream parsing disagree($r3a, $r4a): $s")

      if (r0b != r1b) sys.error(s"CharSequence/String parsing disagree($r0b, $r1b): $s")
      if (r1b != r2b) sys.error(s"String/ByteBuffer parsing disagree($r1b, $r2b): $s")
      if (r2b != r3b) sys.error(s"ByteBuffer/InputStream parsing disagree($r2b, $r3b): $s")
      if (r3b != r4b) sys.error(s"InputStream/ByteArray parsing disagree($r3b, $r4b): $s")
      if (r4b != r5b) sys.error(s"ByteArray/Small-Buffer InputStream parsing disagree($r3b, $r4b): $s")

      if (r0c != r1c) sys.error(s"CharSequence/String parsing disagree($r0c, $r1c): $s")
      if (r1c != r2c) sys.error(s"String/ByteBuffer parsing disagree($r1c, $r2c): $s")
      if (r2c != r3c) sys.error(s"ByteBuffer/InputStream parsing disagree($r2c, $r3c): $s")
      if (r3c != r4c) sys.error(s"InputStream/ByteArray parsing disagree($r3c, $r4c): $s")
      if (r4c != r5c) sys.error(s"ByteArray/Small-Buffer InputStream parsing disagree($r3c, $r4c): $s")

      if (r0d != r1d) sys.error(s"CharSequence/String parsing disagree($r0d, $r1d): $s")
      if (r1d != r2d) sys.error(s"String/ByteBuffer parsing disagree($r1d, $r2d): $s")
      if (r2d != r3d) sys.error(s"ByteBuffer/InputStream parsing disagree($r2d, $r3d): $s")
      if (r3d != r4d) sys.error(s"InputStream/ByteArray parsing disagree($r3d, $r4d): $s")
      if (r4d != r5d) sys.error(s"ByteArray/Small-Buffer InputStream parsing disagree($r3d, $r4d): $s")
    }else{
      assert(r0a.failed.toOption.exists(_.isInstanceOf[ujson.ParsingFailedException]), r0a)
      assert(r1a.failed.toOption.exists(_.isInstanceOf[ujson.ParsingFailedException]), r1a)
      assert(r2a.failed.toOption.exists(_.isInstanceOf[ujson.ParsingFailedException]), r2a)
      assert(r3a.failed.toOption.exists(_.isInstanceOf[ujson.ParsingFailedException]), r3a)
      assert(r4a.failed.toOption.exists(_.isInstanceOf[ujson.ParsingFailedException]), r4a)

      assert(r0b.failed.toOption.exists(_.isInstanceOf[ujson.ParsingFailedException]), r0a)
      assert(r1b.failed.toOption.exists(_.isInstanceOf[ujson.ParsingFailedException]), r1a)
      assert(r2b.failed.toOption.exists(_.isInstanceOf[ujson.ParsingFailedException]), r2a)
      assert(r3b.failed.toOption.exists(_.isInstanceOf[ujson.ParsingFailedException]), r3a)
      assert(r4b.failed.toOption.exists(_.isInstanceOf[ujson.ParsingFailedException]), r4a)

      assert(r0c.failed.toOption.exists(_.isInstanceOf[ujson.ParsingFailedException]), r0a)
      assert(r1c.failed.toOption.exists(_.isInstanceOf[ujson.ParsingFailedException]), r1a)
      assert(r2c.failed.toOption.exists(_.isInstanceOf[ujson.ParsingFailedException]), r2a)
      assert(r3c.failed.toOption.exists(_.isInstanceOf[ujson.ParsingFailedException]), r3a)
      assert(r4c.failed.toOption.exists(_.isInstanceOf[ujson.ParsingFailedException]), r4a)

      assert(r0d.failed.toOption.exists(_.isInstanceOf[ujson.ParsingFailedException]), r0a)
      assert(r1d.failed.toOption.exists(_.isInstanceOf[ujson.ParsingFailedException]), r1a)
      assert(r2d.failed.toOption.exists(_.isInstanceOf[ujson.ParsingFailedException]), r2a)
      assert(r3d.failed.toOption.exists(_.isInstanceOf[ujson.ParsingFailedException]), r3a)
      assert(r4d.failed.toOption.exists(_.isInstanceOf[ujson.ParsingFailedException]), r4a)
    }
  }
}
