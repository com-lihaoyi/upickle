package upack

import utest._
import upickle.core.CountingVisitor
import java.io.{BufferedOutputStream, FileInputStream, FileOutputStream}
import java.nio.file.{Files, Path}
import upack.{MsgPackKeys => MPK}

/**
  * Tests for parsing MsgPack files larger than 2GB.
  *
  * These tests verify the fix for https://github.com/com-lihaoyi/upickle/issues/656
  * which addresses integer overflow when parsing files >2GB.
  */
object LargeFileTests extends TestSuite {

  /**
    * Generates a large MsgPack file with diverse value types and returns
    * the file path along with exact counts of each type emitted.
    *
    * Each element cycles through: numbers, booleans, nulls, strings,
    * nested arrays, nested objects, and binary data.
    */
  case class GeneratedMsgPack(
    path: Path,
    emittedNumbers: Long,
    emittedBooleans: Long,
    emittedNulls: Long,
    emittedStrings: Long,
    emittedArrays: Long,
    emittedObjects: Long,
    emittedBinary: Long
  )

  def generateLargeMsgPackFile(targetBytes: Long): GeneratedMsgPack = {
    val path = Files.createTempFile("large-msgpack-test-", ".msgpack")
    path.toFile.deleteOnExit()

    var emittedNumbers = 0L
    var emittedBooleans = 0L
    var emittedNulls = 0L
    var emittedStrings = 0L
    var emittedArrays = 1L  // 1 for outer array
    var emittedObjects = 0L
    var emittedBinary = 0L

    // Use RandomAccessFile so we can go back and fix the header
    val raf = new java.io.RandomAccessFile(path.toFile, "rw")
    val out = new BufferedOutputStream(new FileOutputStream(raf.getFD), 64 * 1024)
    try {
      var bytesWritten = 0L
      var currentElement = 0L

      // Write array32 header with placeholder (5 bytes: 0xdd + 4 bytes length)
      // We'll come back and fix the element count later
      out.write(MPK.Array32)
      out.write(0)
      out.write(0)
      out.write(0)
      out.write(0)
      bytesWritten += 5

      while (bytesWritten < targetBytes) {
        currentElement += 1

        val bytes: Array[Byte] = (currentElement % 10) match {
          case 0 =>
            // Positive fixint (0-127)
            emittedNumbers += 1
            Array[Byte]((currentElement % 128).toByte)
          case 1 =>
            // Negative fixint
            emittedNumbers += 1
            Array[Byte]((0xe0 | (currentElement % 32)).toByte)
          case 2 =>
            // Int32
            emittedNumbers += 1
            val v = currentElement.toInt
            Array[Byte](
              MPK.Int32.toByte,
              ((v >> 24) & 0xff).toByte,
              ((v >> 16) & 0xff).toByte,
              ((v >> 8) & 0xff).toByte,
              (v & 0xff).toByte
            )
          case 3 =>
            // Float64
            emittedNumbers += 1
            val bits = java.lang.Double.doubleToLongBits(currentElement * 1.5)
            Array[Byte](
              MPK.Float64.toByte,
              ((bits >> 56) & 0xff).toByte,
              ((bits >> 48) & 0xff).toByte,
              ((bits >> 40) & 0xff).toByte,
              ((bits >> 32) & 0xff).toByte,
              ((bits >> 24) & 0xff).toByte,
              ((bits >> 16) & 0xff).toByte,
              ((bits >> 8) & 0xff).toByte,
              (bits & 0xff).toByte
            )
          case 4 =>
            // Boolean: true or false
            emittedBooleans += 1
            if (currentElement % 2 == 0) Array[Byte](MPK.True.toByte)
            else Array[Byte](MPK.False.toByte)
          case 5 =>
            // Nil
            emittedNulls += 1
            Array[Byte](MPK.Nil.toByte)
          case 6 =>
            // Fixstr (short string)
            emittedStrings += 1
            val s = s"s$currentElement"
            val strBytes = s.getBytes("UTF-8")
            Array[Byte]((MPK.FixStrMask | strBytes.length).toByte) ++ strBytes
          case 7 =>
            // Fixarray with 2 integers
            emittedArrays += 1
            emittedNumbers += 2
            val v1 = (currentElement % 100).toByte
            val v2 = ((currentElement + 1) % 100).toByte
            Array[Byte](0x92.toByte, v1, v2)
          case 8 =>
            // Fixmap with 1 key-value pair
            emittedObjects += 1
            emittedStrings += 1
            emittedNumbers += 1
            val k = "k"
            val kBytes = k.getBytes("UTF-8")
            val v = (currentElement % 100).toByte
            Array[Byte](0x81.toByte, (MPK.FixStrMask | kBytes.length).toByte) ++ kBytes ++ Array[Byte](v)
          case 9 =>
            // Bin8 (binary data)
            emittedBinary += 1
            val binData = Array[Byte](0x01, 0x02, 0x03, 0x04)
            Array[Byte](MPK.Bin8.toByte, binData.length.toByte) ++ binData
        }

        out.write(bytes)
        bytesWritten += bytes.length
      }

      // Flush the buffered output before seeking
      out.flush()

      // Go back and fix the array header with the actual element count
      val elementCount = currentElement.toInt
      raf.seek(1) // Skip the 0xdd byte
      raf.write((elementCount >> 24) & 0xff)
      raf.write((elementCount >> 16) & 0xff)
      raf.write((elementCount >> 8) & 0xff)
      raf.write(elementCount & 0xff)
    } finally {
      out.close()
      raf.close()
    }

    GeneratedMsgPack(path, emittedNumbers, emittedBooleans, emittedNulls, emittedStrings, emittedArrays, emittedObjects, emittedBinary)
  }

  def tests = Tests {
    test("parse 5GB synthetic MsgPack stream with diverse types") {
      // Generate 5GB of MsgPack data with diverse value types
      val targetSize = 5L * 1024 * 1024 * 1024 // 5GB
      val generated = generateLargeMsgPackFile(targetSize)

      // Verify file size is >5GB
      val fileSize = Files.size(generated.path)
      println(s"Generated MsgPack file size: ${fileSize} bytes (${fileSize / (1024.0 * 1024 * 1024)} GB)")
      assert(fileSize >= targetSize)

      val visitor = new CountingVisitor()

      // Parse the file using InputStreamMsgPackReader
      val inputStream = new java.io.BufferedInputStream(new FileInputStream(generated.path.toFile))
      try {
        new InputStreamMsgPackReader(inputStream, 64 * 1024, 64 * 1024).parse(visitor)
      } finally {
        inputStream.close()
        Files.deleteIfExists(generated.path)
      }

      // Verify exact counts match what was emitted
      assert(visitor.numberCount == generated.emittedNumbers)
      assert(visitor.boolCount == generated.emittedBooleans)
      assert(visitor.nullCount == generated.emittedNulls)
      assert(visitor.stringCount == generated.emittedStrings)
      assert(visitor.arrayCount == generated.emittedArrays)
      assert(visitor.objectCount == generated.emittedObjects)
      assert(visitor.binaryCount == generated.emittedBinary)

      // After 2GB, indices should start returning -1 (overflow protection)
      // This verifies our fix is working
      assert(visitor.negativeIndexCount > 0)

      println(s"Successfully parsed ${visitor.elementCount} elements from ${fileSize / (1024.0 * 1024 * 1024)} GB MsgPack file")
      println(s"  Numbers: ${visitor.numberCount}")
      println(s"  Booleans: ${visitor.boolCount}")
      println(s"  Nulls: ${visitor.nullCount}")
      println(s"  Strings: ${visitor.stringCount}")
      println(s"  Arrays: ${visitor.arrayCount}")
      println(s"  Objects: ${visitor.objectCount}")
      println(s"  Binary: ${visitor.binaryCount}")
      println(s"Negative index count (expected after 2GB): ${visitor.negativeIndexCount}")
      println(s"Max positive index seen: ${visitor.maxIndex}")
    }

  }
}
