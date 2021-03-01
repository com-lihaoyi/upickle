/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ujson.borer.input

import java.nio.ByteBuffer

import ujson.borer.{ByteAccess, Input}

trait FromByteBufferInput {

  implicit object FromByteBufferProvider extends Input.Provider[ByteBuffer] {
    type Bytes = Array[Byte]
    def byteAccess               = ByteAccess.ForByteArray
    def apply(value: ByteBuffer) = fromByteBuffer(value)
  }

  def fromByteBuffer(value: ByteBuffer): Input[Array[Byte]] = new FromByteBuffer(value)

  final private class FromByteBuffer(buffer: ByteBuffer) extends Input[Array[Byte]] {

    def cursor: Long = buffer.position().toLong

    def unread(numberOfBytes: Int): this.type = {
      buffer.position(buffer.position() - numberOfBytes)
      this
    }

    def readByte(): Byte = buffer.get()

    def readBytePadded(pp: Input.PaddingProvider[Array[Byte]]): Byte =
      if (buffer.hasRemaining) readByte()
      else pp.padByte()

    def readDoubleByteBigEndian(): Char = buffer.getChar

    def readDoubleByteBigEndianPadded(pp: Input.PaddingProvider[Array[Byte]]): Char = {
      val remaining = buffer.remaining
      if (remaining >= 2) readDoubleByteBigEndian()
      else pp.padDoubleByte(remaining)
    }

    def readQuadByteBigEndian(): Int = buffer.getInt()

    def readQuadByteBigEndianPadded(pp: Input.PaddingProvider[Array[Byte]]): Int = {
      val remaining = buffer.remaining
      if (remaining >= 4) readQuadByteBigEndian()
      else pp.padQuadByte(remaining)
    }

    def readOctaByteBigEndian(): Long = buffer.getLong()

    def readOctaByteBigEndianPadded(pp: Input.PaddingProvider[Array[Byte]]): Long = {
      val remaining = buffer.remaining
      if (remaining >= 8) readOctaByteBigEndian()
      else pp.padOctaByte(remaining)
    }

    def readBytes(length: Long, pp: Input.PaddingProvider[Array[Byte]]): Array[Byte] = {
      val remaining = buffer.remaining.toLong
      val len       = math.min(remaining, length).toInt
      val bytes =
        if (len > 0) {
          val bytes = new Array[Byte](len)
          buffer.get(bytes, 0, len)
          bytes
        } else ByteAccess.ForByteArray.empty
      if (length <= remaining) bytes
      else pp.padBytes(bytes, length - remaining)
    }
  }
}
