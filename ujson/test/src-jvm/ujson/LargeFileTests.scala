package ujson

import utest._
import upickle.core.CountingVisitor
import java.io.{BufferedOutputStream, FileInputStream, FileOutputStream, InputStream}
import java.nio.file.{Files, Path}

/**
  * Tests for parsing JSON files larger than 2GB.
  *
  * These tests verify the fix for https://github.com/com-lihaoyi/upickle/issues/656
  * which addresses integer overflow when parsing JSON files >2GB.
  */
object LargeFileTests extends TestSuite {

  /**
    * Generates a large JSON file with diverse value types and returns
    * the file path along with exact counts of each type emitted.
    *
    * Each element cycles through: numbers (int/float), booleans, nulls, strings,
    * nested arrays, and nested objects.
    */
  case class GeneratedJson(
    path: Path,
    emittedNumbers: Long,
    emittedBooleans: Long,
    emittedNulls: Long,
    emittedStrings: Long,
    emittedArrays: Long,
    emittedObjects: Long
  )

  def generateLargeJsonFile(targetBytes: Long): GeneratedJson = {
    val path = Files.createTempFile("large-json-test-", ".json")
    path.toFile.deleteOnExit()

    var emittedNumbers = 0L
    var emittedBooleans = 0L
    var emittedNulls = 0L
    var emittedStrings = 0L
    var emittedArrays = 1L  // 1 for outer array
    var emittedObjects = 0L

    val out = new BufferedOutputStream(new FileOutputStream(path.toFile), 64 * 1024)
    try {
      var bytesWritten = 0L
      var currentElement = 0L

      out.write('[')
      bytesWritten += 1

      while (bytesWritten < targetBytes - 1) {
        currentElement += 1
        val separator = if (currentElement == 1) "" else ","

        val value = (currentElement % 8) match {
          case 0 =>
            emittedNumbers += 1
            currentElement.toString
          case 1 =>
            emittedNumbers += 1
            s"-${currentElement}"
          case 2 =>
            emittedNumbers += 1
            s"${currentElement}.${currentElement % 100}"
          case 3 =>
            emittedBooleans += 1
            if (currentElement % 2 == 0) "true" else "false"
          case 4 =>
            emittedNulls += 1
            "null"
          case 5 =>
            emittedStrings += 1
            s""""string_${currentElement}""""
          case 6 =>
            emittedArrays += 1
            emittedNumbers += 2
            s"[${currentElement}, ${currentElement + 1}]"
          case 7 =>
            emittedObjects += 1
            emittedStrings += 1
            emittedNumbers += 1
            s"""{"k${currentElement}": ${currentElement}}"""
        }

        val bytes = (separator + value).getBytes("UTF-8")
        out.write(bytes)
        bytesWritten += bytes.length
      }

      out.write(']')
    } finally {
      out.close()
    }

    GeneratedJson(path, emittedNumbers, emittedBooleans, emittedNulls, emittedStrings, emittedArrays, emittedObjects)
  }

  def tests = Tests {
    test("parse 5GB synthetic JSON stream with diverse types") {
      // Generate 5GB of JSON data with diverse value types
      val targetSize = 5L * 1024 * 1024 * 1024 // 5GB
      val generated = generateLargeJsonFile(targetSize)

      // Verify file size is >5GB
      val fileSize = Files.size(generated.path)
      println(s"Generated JSON file size: ${fileSize} bytes (${fileSize / (1024.0 * 1024 * 1024)} GB)")
      assert(fileSize >= targetSize)

      val visitor = new CountingVisitor()

      // Parse the file using InputStreamParser
      val inputStream = new java.io.BufferedInputStream(new FileInputStream(generated.path.toFile))
      try {
        new InputStreamParser(inputStream, 64 * 1024, 64 * 1024).parse(visitor)
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

      // After 2GB, indices should start returning -1 (overflow protection)
      // This verifies our fix is working
      assert(visitor.negativeIndexCount > 0)

      println(s"Successfully parsed ${visitor.elementCount} elements from ${fileSize / (1024.0 * 1024 * 1024)} GB JSON file")
      println(s"  Numbers: ${visitor.numberCount}")
      println(s"  Booleans: ${visitor.boolCount}")
      println(s"  Nulls: ${visitor.nullCount}")
      println(s"  Strings: ${visitor.stringCount}")
      println(s"  Arrays: ${visitor.arrayCount}")
      println(s"  Objects: ${visitor.objectCount}")
      println(s"Negative index count (expected after 2GB): ${visitor.negativeIndexCount}")
      println(s"Max positive index seen: ${visitor.maxIndex}")
    }

  }
}
