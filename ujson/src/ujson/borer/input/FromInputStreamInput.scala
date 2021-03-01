/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ujson.borer.input

import java.io.InputStream
import java.util

import ujson.borer.{ByteAccess, Input}

trait FromInputStreamInput { this: FromByteArrayInput with FromIteratorInput =>

  private object FromInputStreamProvider extends Input.Provider[InputStream] {
    type Bytes = Array[Byte]
    def byteAccess                = ByteAccess.ForByteArray
    def apply(value: InputStream) = fromInputStream(value)
  }

  implicit def FromInputStreamProvider[T <: InputStream]: Input.Provider[T] =
    FromInputStreamProvider.asInstanceOf[Input.Provider[T]]

  def fromInputStream(inputStream: InputStream, bufferSize: Int = 16384): Input[Array[Byte]] = {
    if (bufferSize < 256) throw new IllegalArgumentException(s"bufferSize must be >= 256 but was $bufferSize")
    val iterator: Iterator[Input[Array[Byte]]] =
      new Iterator[Input[Array[Byte]]] {
        private[this] val bufA                          = new Array[Byte](bufferSize)
        private[this] val bufB                          = new Array[Byte](bufferSize)
        private[this] var bufSelect: Boolean            = _
        private[this] var nextInput: Input[Array[Byte]] = _

        def hasNext = {
          def tryReadNext() = {
            val buf = if (bufSelect) bufA else bufB
            nextInput = inputStream.read(buf) match {
              case -1 => null
              case `bufferSize` =>
                bufSelect = !bufSelect
                fromByteArray(buf)
              case byteCount => fromByteArray(util.Arrays.copyOfRange(buf, 0, byteCount))
            }
            nextInput ne null
          }
          (nextInput ne null) || tryReadNext()
        }

        def next() =
          if (hasNext) {
            val result = nextInput
            nextInput = null
            result
          } else throw new NoSuchElementException
      }
    fromIterator(iterator)
  }

}
