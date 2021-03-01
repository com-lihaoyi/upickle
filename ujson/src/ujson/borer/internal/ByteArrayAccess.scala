package ujson.borer.internal


import java.nio.ByteOrder

import scala.annotation.tailrec

abstract class ByteArrayAccess {

  def doubleByteBigEndian(byteArray: Array[Byte], ix: Int): Char

  def quadByteBigEndian(byteArray: Array[Byte], ix: Int): Int

  def octaByteBigEndian(byteArray: Array[Byte], ix: Int): Long

  def setDoubleByteBigEndian(byteArray: Array[Byte], ix: Int, value: Char): Unit

  def setQuadByteBigEndian(byteArray: Array[Byte], ix: Int, value: Int): Unit

  def setOctaByteBigEndian(byteArray: Array[Byte], ix: Int, value: Long): Unit

  def shortArrayToByteArray(source: Array[Short], byteOrder: ByteOrder): Array[Byte]
  def intArrayToByteArray(source: Array[Int], byteOrder: ByteOrder): Array[Byte]
  def longArrayToByteArray(source: Array[Long], byteOrder: ByteOrder): Array[Byte]
  def floatArrayToByteArray(source: Array[Float], byteOrder: ByteOrder): Array[Byte]
  def doubleArrayToByteArray(source: Array[Double], byteOrder: ByteOrder): Array[Byte]

  def byteArrayToShortArray(source: Array[Byte], byteOrder: ByteOrder): Array[Short]
  def byteArrayToIntArray(source: Array[Byte], byteOrder: ByteOrder): Array[Int]
  def byteArrayToLongArray(source: Array[Byte], byteOrder: ByteOrder): Array[Long]
  def byteArrayToFloatArray(source: Array[Byte], byteOrder: ByteOrder): Array[Float]
  def byteArrayToDoubleArray(source: Array[Byte], byteOrder: ByteOrder): Array[Double]
}

object ByteArrayAccess {

  final val instance: ByteArrayAccess = {
    val unsafe = ujson.borer.internal.Unsafe.byteArrayAccess
    if (unsafe ne null) unsafe else new Default
  }

  final class Default extends ByteArrayAccess {

    def doubleByteBigEndian(byteArray: Array[Byte], ix: Int): Char =
      ((byteArray(ix) << 8) | byteArray(ix + 1) & 0xFF).toChar

    def quadByteBigEndian(byteArray: Array[Byte], ix: Int): Int =
      byteArray(ix) << 24 |
        (byteArray(ix + 1) & 0xFF) << 16 |
        (byteArray(ix + 2) & 0xFF) << 8 |
        byteArray(ix + 3) & 0xFF

    def octaByteBigEndian(byteArray: Array[Byte], ix: Int): Long = {
      byteArray(ix).toLong << 56 |
        (byteArray(ix + 1) & 0xFFL) << 48 |
        (byteArray(ix + 2) & 0xFFL) << 40 |
        (byteArray(ix + 3) & 0xFFL) << 32 |
        (byteArray(ix + 4) & 0xFFL) << 24 |
        (byteArray(ix + 5) & 0xFFL) << 16 |
        (byteArray(ix + 6) & 0xFFL) << 8 |
        byteArray(ix + 7) & 0xFFL
    }

    def setDoubleByteBigEndian(byteArray: Array[Byte], ix: Int, value: Char): Unit = {
      byteArray(ix + 0) = (value >> 8).toByte
      byteArray(ix + 1) = value.toByte
    }

    def setQuadByteBigEndian(byteArray: Array[Byte], ix: Int, value: Int): Unit = {
      byteArray(ix + 0) = (value >> 24).toByte
      byteArray(ix + 1) = (value >> 16).toByte
      byteArray(ix + 2) = (value >> 8).toByte
      byteArray(ix + 3) = value.toByte
    }

    def setOctaByteBigEndian(byteArray: Array[Byte], ix: Int, value: Long): Unit = {
      byteArray(ix + 0) = (value >> 56).toByte
      byteArray(ix + 1) = (value >> 48).toByte
      byteArray(ix + 2) = (value >> 40).toByte
      byteArray(ix + 3) = (value >> 32).toByte
      byteArray(ix + 4) = (value >> 24).toByte
      byteArray(ix + 5) = (value >> 16).toByte
      byteArray(ix + 6) = (value >> 8).toByte
      byteArray(ix + 7) = value.toByte
    }

    def shortArrayToByteArray(source: Array[Short], byteOrder: ByteOrder): Array[Byte] =
      if (source.length > 0) {
        val bytes = new Array[Byte](source.length << 1)

        @tailrec def recBigEndian(sourceIx: Int, targetIx: Int): Array[Byte] =
          if (sourceIx < source.length) {
            val value = source(sourceIx)
            bytes(targetIx) = (value >>> 8).toByte
            bytes(targetIx + 1) = value.toByte
            recBigEndian(sourceIx + 1, targetIx + 2)
          } else bytes

        @tailrec def recLittleEndian(sourceIx: Int, targetIx: Int): Array[Byte] =
          if (sourceIx < source.length) {
            val value = source(sourceIx)
            bytes(targetIx) = value.toByte
            bytes(targetIx + 1) = (value >>> 8).toByte
            recLittleEndian(sourceIx + 1, targetIx + 2)
          } else bytes

        if (byteOrder == ByteOrder.BIG_ENDIAN) recBigEndian(0, 0) else recLittleEndian(0, 0)
      } else Array.emptyByteArray

    def intArrayToByteArray(source: Array[Int], byteOrder: ByteOrder): Array[Byte] =
      if (source.length > 0) {
        val bytes = new Array[Byte](source.length << 2)

        @tailrec def recBigEndian(sourceIx: Int, targetIx: Int): Array[Byte] =
          if (sourceIx < source.length) {
            val value = source(sourceIx)
            bytes(targetIx) = (value >>> 24).toByte
            bytes(targetIx + 1) = (value >>> 16).toByte
            bytes(targetIx + 2) = (value >>> 8).toByte
            bytes(targetIx + 3) = value.toByte
            recBigEndian(sourceIx + 1, targetIx + 4)
          } else bytes

        @tailrec def recLittleEndian(sourceIx: Int, targetIx: Int): Array[Byte] =
          if (sourceIx < source.length) {
            val value = source(sourceIx)
            bytes(targetIx) = value.toByte
            bytes(targetIx + 1) = (value >>> 8).toByte
            bytes(targetIx + 2) = (value >>> 16).toByte
            bytes(targetIx + 3) = (value >>> 24).toByte
            recLittleEndian(sourceIx + 1, targetIx + 4)
          } else bytes

        if (byteOrder == ByteOrder.BIG_ENDIAN) recBigEndian(0, 0) else recLittleEndian(0, 0)
      } else Array.emptyByteArray

    def longArrayToByteArray(source: Array[Long], byteOrder: ByteOrder): Array[Byte] =
      if (source.length > 0) {
        val bytes = new Array[Byte](source.length << 3)

        @tailrec def recBigEndian(sourceIx: Int, targetIx: Int): Array[Byte] =
          if (sourceIx < source.length) {
            val value = source(sourceIx)
            bytes(targetIx) = (value >>> 56).toByte
            bytes(targetIx + 1) = (value >>> 48).toByte
            bytes(targetIx + 2) = (value >>> 40).toByte
            bytes(targetIx + 3) = (value >>> 32).toByte
            bytes(targetIx + 4) = (value >>> 24).toByte
            bytes(targetIx + 5) = (value >>> 16).toByte
            bytes(targetIx + 6) = (value >>> 8).toByte
            bytes(targetIx + 7) = value.toByte
            recBigEndian(sourceIx + 1, targetIx + 8)
          } else bytes

        @tailrec def recLittleEndian(sourceIx: Int, targetIx: Int): Array[Byte] =
          if (sourceIx < source.length) {
            val value = source(sourceIx)
            bytes(targetIx) = value.toByte
            bytes(targetIx + 1) = (value >>> 8).toByte
            bytes(targetIx + 2) = (value >>> 16).toByte
            bytes(targetIx + 3) = (value >>> 24).toByte
            bytes(targetIx + 4) = (value >>> 32).toByte
            bytes(targetIx + 5) = (value >>> 40).toByte
            bytes(targetIx + 6) = (value >>> 48).toByte
            bytes(targetIx + 7) = (value >>> 56).toByte
            recLittleEndian(sourceIx + 1, targetIx + 8)
          } else bytes

        if (byteOrder == ByteOrder.BIG_ENDIAN) recBigEndian(0, 0) else recLittleEndian(0, 0)
      } else Array.emptyByteArray

    def floatArrayToByteArray(source: Array[Float], byteOrder: ByteOrder): Array[Byte] =
      if (source.length > 0) {
        val bytes = new Array[Byte](source.length << 2)

        @tailrec def recBigEndian(sourceIx: Int, targetIx: Int): Array[Byte] =
          if (sourceIx < source.length) {
            val value = java.lang.Float.floatToIntBits(source(sourceIx))
            bytes(targetIx) = (value >>> 24).toByte
            bytes(targetIx + 1) = (value >>> 16).toByte
            bytes(targetIx + 2) = (value >>> 8).toByte
            bytes(targetIx + 3) = value.toByte
            recBigEndian(sourceIx + 1, targetIx + 4)
          } else bytes

        @tailrec def recLittleEndian(sourceIx: Int, targetIx: Int): Array[Byte] =
          if (sourceIx < source.length) {
            val value = java.lang.Float.floatToIntBits(source(sourceIx))
            bytes(targetIx) = value.toByte
            bytes(targetIx + 1) = (value >>> 8).toByte
            bytes(targetIx + 2) = (value >>> 16).toByte
            bytes(targetIx + 3) = (value >>> 24).toByte
            recLittleEndian(sourceIx + 1, targetIx + 4)
          } else bytes

        if (byteOrder == ByteOrder.BIG_ENDIAN) recBigEndian(0, 0) else recLittleEndian(0, 0)
      } else Array.emptyByteArray

    def doubleArrayToByteArray(source: Array[Double], byteOrder: ByteOrder): Array[Byte] =
      if (source.length > 0) {
        val bytes = new Array[Byte](source.length << 3)

        @tailrec def recBigEndian(sourceIx: Int, targetIx: Int): Array[Byte] =
          if (sourceIx < source.length) {
            val value = java.lang.Double.doubleToLongBits(source(sourceIx))
            bytes(targetIx) = (value >>> 56).toByte
            bytes(targetIx + 1) = (value >>> 48).toByte
            bytes(targetIx + 2) = (value >>> 40).toByte
            bytes(targetIx + 3) = (value >>> 32).toByte
            bytes(targetIx + 4) = (value >>> 24).toByte
            bytes(targetIx + 5) = (value >>> 16).toByte
            bytes(targetIx + 6) = (value >>> 8).toByte
            bytes(targetIx + 7) = value.toByte
            recBigEndian(sourceIx + 1, targetIx + 8)
          } else bytes

        @tailrec def recLittleEndian(sourceIx: Int, targetIx: Int): Array[Byte] =
          if (sourceIx < source.length) {
            val value = java.lang.Double.doubleToLongBits(source(sourceIx))
            bytes(targetIx) = value.toByte
            bytes(targetIx + 1) = (value >>> 8).toByte
            bytes(targetIx + 2) = (value >>> 16).toByte
            bytes(targetIx + 3) = (value >>> 24).toByte
            bytes(targetIx + 4) = (value >>> 32).toByte
            bytes(targetIx + 5) = (value >>> 40).toByte
            bytes(targetIx + 6) = (value >>> 48).toByte
            bytes(targetIx + 7) = (value >>> 56).toByte
            recLittleEndian(sourceIx + 1, targetIx + 8)
          } else bytes

        if (byteOrder == ByteOrder.BIG_ENDIAN) recBigEndian(0, 0) else recLittleEndian(0, 0)
      } else Array.emptyByteArray

    def byteArrayToShortArray(source: Array[Byte], byteOrder: ByteOrder): Array[Short] =
      if (source.length > 0) {
        if ((source.length & 1) != 0)
          throw new IllegalArgumentException(s"source Array[Byte] has illegal length: ${source.length}")
        val target = new Array[Short](source.length >> 1)

        @tailrec def recBigEndian(sourceIx: Int, targetIx: Int): Array[Short] =
          if (targetIx < target.length) {
            target(targetIx) = ((source(sourceIx).toInt << 8) | (source(sourceIx + 1) & 0xFF)).toShort
            recBigEndian(sourceIx + 2, targetIx + 1)
          } else target

        @tailrec def recLittleEndian(sourceIx: Int, targetIx: Int): Array[Short] =
          if (targetIx < target.length) {
            target(targetIx) = ((source(sourceIx) & 0xFF) | (source(sourceIx + 1).toInt << 8)).toShort
            recLittleEndian(sourceIx + 2, targetIx + 1)
          } else target

        if (byteOrder == ByteOrder.BIG_ENDIAN) recBigEndian(0, 0) else recLittleEndian(0, 0)
      } else Array.emptyShortArray

    def byteArrayToIntArray(source: Array[Byte], byteOrder: ByteOrder): Array[Int] =
      if (source.length > 0) {
        if ((source.length & 3) != 0)
          throw new IllegalArgumentException(s"source Array[Byte] has illegal length: ${source.length}")
        val target = new Array[Int](source.length >> 2)

        @tailrec def recBigEndian(sourceIx: Int, targetIx: Int): Array[Int] =
          if (targetIx < target.length) {
            target(targetIx) = (source(sourceIx).toInt << 24) |
              ((source(sourceIx + 1) & 0xFF) << 16) |
              ((source(sourceIx + 2) & 0xFF) << 8) |
              (source(sourceIx + 3) & 0xFF)
            recBigEndian(sourceIx + 4, targetIx + 1)
          } else target

        @tailrec def recLittleEndian(sourceIx: Int, targetIx: Int): Array[Int] =
          if (targetIx < target.length) {
            target(targetIx) = (source(sourceIx) & 0xFF) |
              ((source(sourceIx + 1) & 0xFF) << 8) |
              ((source(sourceIx + 2) & 0xFF) << 16) |
              (source(sourceIx + 3).toInt << 24)
            recLittleEndian(sourceIx + 4, targetIx + 1)
          } else target

        if (byteOrder == ByteOrder.BIG_ENDIAN) recBigEndian(0, 0) else recLittleEndian(0, 0)
      } else Array.emptyIntArray

    def byteArrayToLongArray(source: Array[Byte], byteOrder: ByteOrder): Array[Long] =
      if (source.length > 0) {
        if ((source.length & 7) != 0)
          throw new IllegalArgumentException(s"source Array[Byte] has illegal length: ${source.length}")
        val target = new Array[Long](source.length >> 3)

        @tailrec def recBigEndian(sourceIx: Int, targetIx: Int): Array[Long] =
          if (targetIx < target.length) {
            target(targetIx) = (source(sourceIx).toLong << 56) |
              ((source(sourceIx + 1) & 0xFFL) << 48) |
              ((source(sourceIx + 2) & 0xFFL) << 40) |
              ((source(sourceIx + 3) & 0xFFL) << 32) |
              ((source(sourceIx + 4) & 0xFFL) << 24) |
              ((source(sourceIx + 5) & 0xFFL) << 16) |
              ((source(sourceIx + 6) & 0xFFL) << 8) |
              (source(sourceIx + 7) & 0xFFL)
            recBigEndian(sourceIx + 8, targetIx + 1)
          } else target

        @tailrec def recLittleEndian(sourceIx: Int, targetIx: Int): Array[Long] =
          if (targetIx < target.length) {
            target(targetIx) = (source(sourceIx) & 0xFFL) |
              ((source(sourceIx + 1) & 0xFFL) << 8) |
              ((source(sourceIx + 2) & 0xFFL) << 16) |
              ((source(sourceIx + 3) & 0xFFL) << 24) |
              ((source(sourceIx + 4) & 0xFFL) << 32) |
              ((source(sourceIx + 5) & 0xFFL) << 40) |
              ((source(sourceIx + 6) & 0xFFL) << 48) |
              (source(sourceIx + 7).toLong << 56)
            recLittleEndian(sourceIx + 8, targetIx + 1)
          } else target

        if (byteOrder == ByteOrder.BIG_ENDIAN) recBigEndian(0, 0) else recLittleEndian(0, 0)
      } else Array.emptyLongArray

    def byteArrayToFloatArray(source: Array[Byte], byteOrder: ByteOrder): Array[Float] =
      if (source.length > 0) {
        if ((source.length & 3) != 0)
          throw new IllegalArgumentException(s"source Array[Byte] has illegal length: ${source.length}")
        val target = new Array[Float](source.length >> 2)

        @tailrec def recBigEndian(sourceIx: Int, targetIx: Int): Array[Float] =
          if (targetIx < target.length) {
            target(targetIx) = java.lang.Float.intBitsToFloat {
              (source(sourceIx).toInt << 24) |
                ((source(sourceIx + 1) & 0xFF) << 16) |
                ((source(sourceIx + 2) & 0xFF) << 8) |
                (source(sourceIx + 3) & 0xFF)
            }
            recBigEndian(sourceIx + 4, targetIx + 1)
          } else target

        @tailrec def recLittleEndian(sourceIx: Int, targetIx: Int): Array[Float] =
          if (targetIx < target.length) {
            target(targetIx) = java.lang.Float.intBitsToFloat {
              (source(sourceIx) & 0xFF) |
                ((source(sourceIx + 1) & 0xFF) << 8) |
                ((source(sourceIx + 2) & 0xFF) << 16) |
                (source(sourceIx + 3).toInt << 24)
            }
            recLittleEndian(sourceIx + 4, targetIx + 1)
          } else target

        if (byteOrder == ByteOrder.BIG_ENDIAN) recBigEndian(0, 0) else recLittleEndian(0, 0)
      } else Array.emptyFloatArray

    def byteArrayToDoubleArray(source: Array[Byte], byteOrder: ByteOrder): Array[Double] =
      if (source.length > 0) {
        if ((source.length & 7) != 0)
          throw new IllegalArgumentException(s"source Array[Byte] has illegal length: ${source.length}")
        val target = new Array[Double](source.length >> 3)

        @tailrec def recBigEndian(sourceIx: Int, targetIx: Int): Array[Double] =
          if (targetIx < target.length) {
            target(targetIx) = java.lang.Double.longBitsToDouble {
              (source(sourceIx).toLong << 56) |
                ((source(sourceIx + 1) & 0xFFL) << 48) |
                ((source(sourceIx + 2) & 0xFFL) << 40) |
                ((source(sourceIx + 3) & 0xFFL) << 32) |
                ((source(sourceIx + 4) & 0xFFL) << 24) |
                ((source(sourceIx + 5) & 0xFFL) << 16) |
                ((source(sourceIx + 6) & 0xFFL) << 8) |
                (source(sourceIx + 7) & 0xFFL)
            }
            recBigEndian(sourceIx + 8, targetIx + 1)
          } else target

        @tailrec def recLittleEndian(sourceIx: Int, targetIx: Int): Array[Double] =
          if (targetIx < target.length) {
            target(targetIx) = java.lang.Double.longBitsToDouble {
              (source(sourceIx) & 0xFFL) |
                ((source(sourceIx + 1) & 0xFFL) << 8) |
                ((source(sourceIx + 2) & 0xFFL) << 16) |
                ((source(sourceIx + 3) & 0xFFL) << 24) |
                ((source(sourceIx + 4) & 0xFFL) << 32) |
                ((source(sourceIx + 5) & 0xFFL) << 40) |
                ((source(sourceIx + 6) & 0xFFL) << 48) |
                (source(sourceIx + 7).toLong << 56)
            }
            recLittleEndian(sourceIx + 8, targetIx + 1)
          } else target

        if (byteOrder == ByteOrder.BIG_ENDIAN) recBigEndian(0, 0) else recLittleEndian(0, 0)
      } else Array.emptyDoubleArray
  }
}