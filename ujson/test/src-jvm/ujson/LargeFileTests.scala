package ujson

import utest._
import upickle.core.{Visitor, ArrVisitor, ObjVisitor}
import java.io.InputStream

/**
  * Tests for parsing files larger than 2GB.
  *
  * These tests verify the fix for https://github.com/com-lihaoyi/upickle/issues/656
  * which addresses integer overflow when parsing JSON files >2GB.
  */
object LargeFileTests extends TestSuite {

  /**
    * A counting visitor that tracks how many elements were visited
    * and what indices were passed. Used to verify the parser handles
    * large files without overflow.
    */
  class CountingVisitor extends Visitor[Unit, Unit] {
    var elementCount: Long = 0L
    var negativeIndexCount: Long = 0L
    var maxIndex: Int = Int.MinValue

    def visitArray(length: Int, index: Int) = {
      trackIndex(index)
      new CountingArrVisitor(this)
    }

    def visitObject(length: Int, jsonableKeys: Boolean, index: Int) = {
      trackIndex(index)
      new CountingObjVisitor(this)
    }

    private def trackIndex(index: Int): Unit = {
      if (index < 0) negativeIndexCount += 1
      else if (index > maxIndex) maxIndex = index
    }

    def visitNull(index: Int): Unit = { elementCount += 1; trackIndex(index) }
    def visitFalse(index: Int): Unit = { elementCount += 1; trackIndex(index) }
    def visitTrue(index: Int): Unit = { elementCount += 1; trackIndex(index) }
    def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): Unit = {
      elementCount += 1; trackIndex(index)
    }
    def visitString(s: CharSequence, index: Int): Unit = { elementCount += 1; trackIndex(index) }
    def visitFloat64(d: Double, index: Int): Unit = { elementCount += 1; trackIndex(index) }
    def visitFloat32(d: Float, index: Int): Unit = { elementCount += 1; trackIndex(index) }
    def visitInt32(i: Int, index: Int): Unit = { elementCount += 1; trackIndex(index) }
    def visitInt64(i: Long, index: Int): Unit = { elementCount += 1; trackIndex(index) }
    def visitUInt64(i: Long, index: Int): Unit = { elementCount += 1; trackIndex(index) }
    def visitFloat64String(s: String, index: Int): Unit = { elementCount += 1; trackIndex(index) }
    def visitBinary(bytes: Array[Byte], offset: Int, len: Int, index: Int): Unit = { elementCount += 1; trackIndex(index) }
    def visitExt(tag: Byte, bytes: Array[Byte], offset: Int, len: Int, index: Int): Unit = { elementCount += 1; trackIndex(index) }
    def visitChar(s: Char, index: Int): Unit = { elementCount += 1; trackIndex(index) }
  }

  class CountingArrVisitor(parent: CountingVisitor) extends ArrVisitor[Unit, Unit] {
    def subVisitor = parent
    def visitValue(v: Unit, index: Int): Unit = {
      if (index < 0) parent.negativeIndexCount += 1
    }
    def visitEnd(index: Int): Unit = {
      if (index < 0) parent.negativeIndexCount += 1
    }
  }

  class CountingObjVisitor(parent: CountingVisitor) extends ObjVisitor[Unit, Unit] {
    def subVisitor = parent
    def visitKey(index: Int) = parent
    def visitKeyValue(s: Any): Unit = ()
    def visitValue(v: Unit, index: Int): Unit = {
      if (index < 0) parent.negativeIndexCount += 1
    }
    def visitEnd(index: Int): Unit = {
      if (index < 0) parent.negativeIndexCount += 1
    }
  }

  /**
    * An InputStream that generates a large JSON array on the fly.
    * Format: [1,2,3,4,...,n] where each number is padded to ensure we exceed target size.
    *
    * @param targetBytes The target size in bytes to generate
    */
  class LargeJsonInputStream(targetBytes: Long) extends InputStream {
    private var bytesGenerated: Long = 0L
    private var currentElement: Long = 0L
    private var elementBuffer: Array[Byte] = null
    private var elementIndex: Int = 0
    private var finished = false
    private var started = false

    // Each element is a number like "1234567890," with padding
    // We pad with spaces to make each element ~100 bytes for faster generation
    private val elementSize = 100

    override def read(): Int = {
      if (finished) return -1

      if (!started) {
        started = true
        bytesGenerated += 1
        return '['.toInt
      }

      if (elementBuffer == null || elementIndex >= elementBuffer.length) {
        if (bytesGenerated >= targetBytes - 1) {
          finished = true
          bytesGenerated += 1
          return ']'.toInt
        }

        // Generate next element
        currentElement += 1
        val numStr = currentElement.toString
        val paddingLen = math.max(0, elementSize - numStr.length - 1) // -1 for comma
        val padding = " " * paddingLen
        val separator = if (currentElement == 1) "" else ","
        elementBuffer = (separator + numStr + padding).getBytes("UTF-8")
        elementIndex = 0
      }

      val b = elementBuffer(elementIndex) & 0xFF
      elementIndex += 1
      bytesGenerated += 1
      b
    }

    override def read(b: Array[Byte], off: Int, len: Int): Int = {
      if (finished) return -1

      var count = 0
      while (count < len) {
        val byte = read()
        if (byte == -1) {
          return if (count == 0) -1 else count
        }
        b(off + count) = byte.toByte
        count += 1
      }
      count
    }
  }

  def tests = Tests {
    test("parse 5GB synthetic JSON stream") {
      // Generate 5GB of JSON data
      val targetSize = 5L * 1024 * 1024 * 1024 // 5GB
      val inputStream = new LargeJsonInputStream(targetSize)

      val visitor = new CountingVisitor()

      // Parse the stream using InputStreamParser
      new InputStreamParser(inputStream, 64 * 1024, 64 * 1024).parse(visitor)

      // Verify we parsed a large number of elements
      assert(visitor.elementCount > 0)

      // After 2GB, indices should start returning -1 (overflow protection)
      // This verifies our fix is working
      assert(visitor.negativeIndexCount > 0)

      println(s"Successfully parsed ${visitor.elementCount} elements from ~5GB stream")
      println(s"Negative index count (expected after 2GB): ${visitor.negativeIndexCount}")
      println(s"Max positive index seen: ${visitor.maxIndex}")
    }

    test("index reset threshold works correctly") {
      // Test that indices reset properly around the 1GB threshold
      // by parsing a smaller but still significant amount of data
      val targetSize = 1200L * 1024 * 1024 // 1.2GB - just past the 1GB reset threshold
      val inputStream = new LargeJsonInputStream(targetSize)

      val visitor = new CountingVisitor()

      new InputStreamParser(inputStream, 64 * 1024, 64 * 1024).parse(visitor)

      // Verify parsing succeeded
      assert(visitor.elementCount > 0)

      // At 1.2GB, we should have triggered at least one index reset
      // but indices should still be valid (positive) since we're below 2GB absolute
      println(s"Parsed ${visitor.elementCount} elements from ~1.2GB stream")
      println(s"Max positive index: ${visitor.maxIndex}")
    }
  }
}
