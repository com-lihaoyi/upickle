/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ujson.borer.input

import ujson.borer._
import ujson.borer.internal.Util.RichIterator

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait FromIteratorInput {

  implicit def FromIteratorProvider[T](implicit p: Input.Provider[T]): Input.Provider[Iterator[T]] =
    new Input.Provider[Iterator[T]] {
      type Bytes = p.Bytes
      implicit def byteAccess       = p.byteAccess
      def apply(value: Iterator[T]) = fromIterator[p.Bytes](value.map(p(_)))
    }

  def fromIterator[Bytes: ByteAccess](value: Iterator[Input[Bytes]]): Input[Bytes] =
    new FromIterator(value)

  final private class FromIterator[Bytes](private[this] var inputIterator: Iterator[Input[Bytes]])(
      implicit byteAccess: ByteAccess[Bytes])
      extends Input.PaddingProvider[Bytes] with Input[Bytes] {

    private[this] var history                                            = List.empty[Input[Bytes]]
    private[this] var previous: Input[Bytes]                             = _
    private[this] var current: Input[Bytes]                              = if (inputIterator.hasNext) inputIterator.next() else null
    private[this] var currentStart: Long                                 = _
    private[this] var outerPaddingProvider: Input.PaddingProvider[Bytes] = _
    private[this] var padBytesRecursion                                  = false
    private[this] var padBytesRecursionRest: Bytes                       = _
    private[this] var padBytesRecursionMissing: Long                     = _

    def cursor: Long = currentStart + cursorOf(current)

    def unread(numberOfBytes: Int): this.type = {
      @tailrec def rollBackOne(
          input: Input[Bytes],
          inputEnd: Long,
          target: Long,
          remainingInputs: Iterator[Input[Bytes]]): Iterator[Input[Bytes]] = {

        val inputCursor = input.cursor
        val inputStart  = inputEnd - inputCursor
        if (inputStart <= target) {
          input.unread((inputEnd - target).toInt)
          previous = history match {
            case head :: tail => history = tail; head
            case Nil          => null
          }
          current = input
          currentStart = inputStart
          remainingInputs
        } else {
          input.unread(inputCursor.toInt)
          history match {
            case head :: tail =>
              history = tail
              rollBackOne(head, inputStart, target, input +: remainingInputs)
            case Nil => throw new IllegalStateException // rollback too far?
          }
        }
      }

      val currentCursor = cursorOf(current)
      if (currentCursor < numberOfBytes) {
        current.unread(currentCursor.toInt)
        val target = currentStart + currentCursor - numberOfBytes
        inputIterator = rollBackOne(previous, currentStart, target, current +: inputIterator)
      } else current.unread(numberOfBytes)
      this
    }

    def readByte(): Byte = current.readByte()

    def readBytePadded(pp: Input.PaddingProvider[Bytes]): Byte = {
      outerPaddingProvider = pp
      if (current ne null) current.readBytePadded(this)
      else pp.padByte()
    }

    def padByte(): Byte =
      if (inputIterator.hasNext) {
        fetchNext(0)
        current.readBytePadded(this)
      } else outerPaddingProvider.padByte()

    def readDoubleByteBigEndian(): Char = current.readDoubleByteBigEndian()

    def readDoubleByteBigEndianPadded(pp: Input.PaddingProvider[Bytes]): Char = {
      outerPaddingProvider = pp
      if (current ne null) current.readDoubleByteBigEndianPadded(this)
      else pp.padDoubleByte(0)
    }

    def padDoubleByte(remaining: Int): Char =
      if (inputIterator.hasNext) {
        fetchNext(remaining)
        if (remaining < 1) current.readDoubleByteBigEndianPadded(this)
        else ((previous.readByte() << 8) | current.readBytePadded(this) & 0xFF).toChar
      } else outerPaddingProvider.padDoubleByte(remaining)

    def readQuadByteBigEndian(): Int = current.readQuadByteBigEndian()

    def readQuadByteBigEndianPadded(pp: Input.PaddingProvider[Bytes]): Int = {
      outerPaddingProvider = pp
      if (current ne null) current.readQuadByteBigEndianPadded(this)
      else pp.padQuadByte(0)
    }

    def padQuadByte(remaining: Int): Int =
      if (inputIterator.hasNext) {
        fetchNext(remaining)
        remaining match {
          case 0 => current.readQuadByteBigEndianPadded(this)
          case 1 =>
            (previous.readByte() << 24) |
              (current.readDoubleByteBigEndianPadded(this) << 8) |
              current.readBytePadded(this) & 0xFF
          case 2 => (previous.readDoubleByteBigEndian() << 16) | current.readDoubleByteBigEndianPadded(this)
          case 3 =>
            (previous.readDoubleByteBigEndian() << 16) |
              ((previous.readByte() & 0xFF) << 8) |
              current.readBytePadded(this) & 0xFF
          case _ => throw new IllegalStateException
        }
      } else outerPaddingProvider.padQuadByte(remaining)

    def readOctaByteBigEndian(): Long = current.readOctaByteBigEndian()

    def readOctaByteBigEndianPadded(pp: Input.PaddingProvider[Bytes]): Long = {
      outerPaddingProvider = pp
      if (current ne null) current.readOctaByteBigEndianPadded(this)
      else pp.padOctaByte(0)
    }

    def padOctaByte(remaining: Int): Long =
      if (inputIterator.hasNext) {
        fetchNext(remaining)
        remaining match {
          case 0 => current.readOctaByteBigEndianPadded(this)
          case 1 =>
            (previous.readByte().toLong << 56) |
              ((current.readQuadByteBigEndianPadded(this) & 0xFFFFFFFFL) << 24) |
              ((current.readDoubleByteBigEndianPadded(this) & 0xFFFFL) << 8) |
              current.readBytePadded(this) & 0xFFL
          case 2 =>
            (previous.readDoubleByteBigEndian().toLong << 48) |
              ((current.readQuadByteBigEndianPadded(this) & 0xFFFFFFFFL) << 16) |
              current.readDoubleByteBigEndianPadded(this) & 0xFFFFL
          case 3 =>
            (previous.readDoubleByteBigEndian().toLong << 48) |
              ((previous.readByte() & 0xFFL) << 40) |
              ((current.readQuadByteBigEndianPadded(this) & 0xFFFFFFFFL) << 8) |
              current.readBytePadded(this) & 0xFFL
          case 4 =>
            (previous.readQuadByteBigEndian().toLong << 32) | (current.readQuadByteBigEndianPadded(this) & 0xFFFFFFFFL)
          case 5 =>
            (previous.readQuadByteBigEndian().toLong << 32) |
              ((previous.readByte() & 0xFFL) << 24) |
              ((current.readDoubleByteBigEndianPadded(this) & 0xFFFFL) << 8) |
              current.readBytePadded(this) & 0xFFL
          case 6 =>
            (previous.readQuadByteBigEndian().toLong << 32) |
              ((previous.readDoubleByteBigEndian() & 0xFFFFL) << 16) |
              current.readDoubleByteBigEndianPadded(this) & 0xFFFFL
          case 7 =>
            (previous.readQuadByteBigEndian().toLong << 32) |
              ((previous.readDoubleByteBigEndian() & 0xFFFFL) << 16) |
              ((previous.readByte() & 0xFFL) << 8) |
              current.readBytePadded(this) & 0xFFL
          case _ => throw new IllegalStateException
        }
      } else outerPaddingProvider.padOctaByte(remaining)

    def readBytes(length: Long, pp: Input.PaddingProvider[Bytes]): Bytes = {
      outerPaddingProvider = pp
      if (current ne null) current.readBytes(length, this)
      else pp.padBytes(byteAccess.empty, length)
    }

    def padBytes(rest: Bytes, missing: Long): Bytes =
      if (!padBytesRecursion) {
        padBytesRecursion = true

        @tailrec def rec(result: Bytes, missing: Long): Bytes = {
          padBytesRecursionRest = byteAccess.empty
          padBytesRecursionMissing = 0L
          if (inputIterator.hasNext) {
            fetchNext(0)
            val nextBytes = current.readBytes(missing, this)
            if (padBytesRecursionMissing == 0) byteAccess.concat(result, nextBytes)
            else rec(byteAccess.concat(result, padBytesRecursionRest), padBytesRecursionMissing)
          } else outerPaddingProvider.padBytes(result, missing)
        }

        val result = rec(rest, missing)
        padBytesRecursion = false
        result
      } else {
        padBytesRecursionRest = rest
        padBytesRecursionMissing = missing
        byteAccess.empty
      }

    private def fetchNext(remaining: Int): Unit = {
      val currentCursor = cursorOf(current)
      val cursor        = currentStart + currentCursor

      history = if ((previous ne null) && currentCursor < 256) {
        // keep the prefix of the history that is required to maintain a total history length of >= 256 bytes
        @tailrec def rec(rest: List[Input[Bytes]], size: Long, buf: ListBuffer[Input[Bytes]]): List[Input[Bytes]] =
          if (size < 256 && rest.nonEmpty) rec(rest.tail, size + cursorOf(rest.head), buf += rest.head)
          else buf.toList
        rec(history, cursorOf(previous), new ListBuffer[Input[Bytes]] += previous)
      } else Nil

      previous = current
      currentStart = cursor + remaining.toLong
      current = inputIterator.next()
    }

    private def cursorOf(input: Input[Bytes]): Long = if (input ne null) input.cursor else 0L
  }
}
