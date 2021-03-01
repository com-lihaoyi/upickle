/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ujson.borer.output

import java.nio.ByteBuffer

import ujson.borer.{ByteAccess, Output}
import ujson.borer.Output.ToTypeProvider
import ujson.borer.internal.ByteBufferCache

import scala.annotation.tailrec

trait ToByteBufferOutput {

  implicit object ToByteBufferProvider extends ToTypeProvider[ByteBuffer] {
    type Out = ToByteBuffer
    def apply(bufferSize: Int, allowBufferCaching: Boolean) = new ToByteBuffer(bufferSize, allowBufferCaching)
  }

  /**
    * Default, mutable implementation for serializing to [[java.nio.ByteBuffer]] instances.
    */
  final class ToByteBuffer(bufferSize: Int, allowBufferCaching: Boolean) extends Output {

    private[this] var currentChunkBuffer: ByteBuffer = _
    private[this] var rootChunk: Chunk               = _
    private[this] var currentChunk: Chunk            = _
    private[this] var fullChunksSize: Long           = _

    type Self   = ToByteBuffer
    type Result = ByteBuffer

    @inline def size: Long =
      if (currentChunkBuffer ne null) fullChunksSize + currentChunkBuffer.position().toLong else 0L

    def writeByte(byte: Byte): this.type = {
      ensureBufferAllocated()
      if (!currentChunkBuffer.hasRemaining) appendChunk()
      currentChunkBuffer.put(byte)
      this
    }

    def writeBytes(a: Byte, b: Byte): this.type = {
      ensureBufferAllocated()
      if (currentChunkBuffer.remaining >= 2) {
        currentChunkBuffer.put(a)
        currentChunkBuffer.put(b)
        this
      } else writeByte(a).writeByte(b)
    }

    override def writeShort(value: Short): this.type = {
      ensureBufferAllocated()
      if (currentChunkBuffer.remaining >= 2) {
        currentChunkBuffer.putShort(value)
        this
      } else writeByte((value >> 8).toByte).writeByte(value.toByte)
    }

    def writeBytes(a: Byte, b: Byte, c: Byte): this.type = {
      ensureBufferAllocated()
      if (currentChunkBuffer.remaining >= 3) {
        currentChunkBuffer.put(a)
        currentChunkBuffer.put(b)
        currentChunkBuffer.put(c)
        this
      } else writeByte(a).writeByte(b).writeByte(c)
    }

    def writeBytes(a: Byte, b: Byte, c: Byte, d: Byte): this.type = {
      ensureBufferAllocated()
      if (currentChunkBuffer.remaining >= 4) {
        currentChunkBuffer.put(a)
        currentChunkBuffer.put(b)
        currentChunkBuffer.put(c)
        currentChunkBuffer.put(d)
        this
      } else writeByte(a).writeByte(b).writeByte(c).writeByte(d)
    }

    override def writeInt(value: Int) = {
      ensureBufferAllocated()
      if (currentChunkBuffer.remaining >= 4) {
        currentChunkBuffer.putInt(value)
        this
      } else writeShort((value >> 16).toShort).writeShort(value.toShort)
    }

    override def writeLong(value: Long) = {
      ensureBufferAllocated()
      if (currentChunkBuffer.remaining >= 8) {
        currentChunkBuffer.putLong(value)
        this
      } else writeInt((value >> 32).toInt).writeInt(value.toInt)
    }

    def writeBytes[Bytes](bytes: Bytes)(implicit byteAccess: ByteAccess[Bytes]): this.type = {
      @tailrec def rec(rest: Bytes): this.type = {
        val newRest = byteAccess.copyToByteBuffer(rest, currentChunkBuffer)
        if (!byteAccess.isEmpty(newRest)) {
          appendChunk()
          rec(newRest)
        } else this
      }
      ensureBufferAllocated()
      rec(bytes)
    }

    def result(): ByteBuffer = {
      ensureBufferAllocated()
      val longSize = size
      val intSize  = longSize.toInt
      if (intSize != longSize) {
        throw new ujson.borer.Error.Overflow(this, f"Output size of $longSize%,d bytes too large for ByteBuffer")
      }
      val buf = ByteBuffer.allocate(intSize)

      @tailrec def rec(chunk: Chunk): ByteBuffer =
        if (chunk ne null) {
          buf.put(chunk.buffer.flip().asInstanceOf[ByteBuffer])
          rec(chunk.next)
        } else {
          if (allowBufferCaching) ByteBufferCache.release(currentChunkBuffer)
          currentChunkBuffer = null
          buf.flip().asInstanceOf[ByteBuffer]
        }

      rec(rootChunk)
    }

    @inline private def ensureBufferAllocated(): Unit =
      if (currentChunkBuffer eq null) allocateBuffer()

    private def allocateBuffer(): Unit = {
      currentChunkBuffer =
        if (allowBufferCaching) ByteBufferCache.acquire(bufferSize) else ByteBuffer.allocate(bufferSize)
      rootChunk = new Chunk(currentChunkBuffer, next = null)
      currentChunk = rootChunk
      fullChunksSize = 0L
    }

    private def appendChunk(): Unit = {
      currentChunkBuffer = ByteBuffer.allocate(bufferSize)
      val newChunk = new Chunk(currentChunkBuffer, null)
      currentChunk.next = newChunk
      currentChunk = newChunk
      fullChunksSize += bufferSize.toLong
    }

    override def toString = s"Output.ToByteBuffer index $size"
  }

  final private class Chunk(val buffer: ByteBuffer, var next: Chunk)
}
