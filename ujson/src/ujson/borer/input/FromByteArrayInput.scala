/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ujson.borer.input

import ujson.borer.{ByteAccess, Input}
import ujson.borer.internal.ByteArrayAccess

trait FromByteArrayInput {

  implicit object FromByteArrayProvider extends Input.Provider[Array[Byte]] {
    type Bytes = Array[Byte]
    def byteAccess                = ByteAccess.ForByteArray
    def apply(value: Array[Byte]) = fromByteArray(value)
  }

  def fromByteArray(value: Array[Byte]): Input[Array[Byte]] = new FromByteArray(value)

  final private class FromByteArray(byteArray: Array[Byte]) extends Input[Array[Byte]] {
    private[this] var _cursor: Int = _

    def cursor: Long = _cursor.toLong

    def unread(numberOfBytes: Int): this.type = {
      _cursor -= numberOfBytes
      this
    }

    def readByte(): Byte = {
      val c = _cursor
      _cursor = c + 1
      byteArray(c)
    }

    def readBytePadded(pp: Input.PaddingProvider[Array[Byte]]): Byte =
      if (_cursor >= byteArray.length) pp.padByte() else readByte()

    def readDoubleByteBigEndian(): Char = {
      val c = _cursor
      _cursor = c + 2
      ByteArrayAccess.instance.doubleByteBigEndian(byteArray, c)
    }

    def readDoubleByteBigEndianPadded(pp: Input.PaddingProvider[Array[Byte]]): Char = {
      val remaining = byteArray.length - _cursor
      if (remaining >= 2) readDoubleByteBigEndian()
      else pp.padDoubleByte(remaining)
    }

    def readQuadByteBigEndian(): Int = {
      val c = _cursor
      _cursor = c + 4
      ByteArrayAccess.instance.quadByteBigEndian(byteArray, c)
    }

    def readQuadByteBigEndianPadded(pp: Input.PaddingProvider[Array[Byte]]): Int = {
      val remaining = byteArray.length - _cursor
      if (remaining >= 4) readQuadByteBigEndian()
      else pp.padQuadByte(remaining)
    }

    def readOctaByteBigEndian(): Long = {
      val c = _cursor
      _cursor = c + 8
      ByteArrayAccess.instance.octaByteBigEndian(byteArray, c)
    }

    def readOctaByteBigEndianPadded(pp: Input.PaddingProvider[Array[Byte]]): Long = {
      val remaining = byteArray.length - _cursor
      if (remaining >= 8) readOctaByteBigEndian()
      else pp.padOctaByte(remaining)
    }

    def readBytes(length: Long, pp: Input.PaddingProvider[Array[Byte]]): Array[Byte] = {
      val remaining = (byteArray.length - _cursor).toLong
      val len       = math.min(remaining, length).toInt
      val bytes =
        if (len > 0) {
          val result = new Array[Byte](len)
          val c      = _cursor
          _cursor = c + len
          System.arraycopy(byteArray, c, result, 0, len)
          result
        } else Array.emptyByteArray
      if (length <= remaining) bytes
      else pp.padBytes(bytes, length - remaining)
    }
  }
}
