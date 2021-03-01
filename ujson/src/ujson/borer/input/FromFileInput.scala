/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ujson.borer.input

import java.io.{File, FileInputStream}
import java.nio.file.Files

import ujson.borer.{ByteAccess, Input}

trait FromFileInput { this: FromByteArrayInput with FromInputStreamInput =>

  implicit object FromFileProvider extends Input.Provider[File] {
    type Bytes = Array[Byte]
    def byteAccess         = ByteAccess.ForByteArray
    def apply(value: File) = fromFile(value)
  }

  def fromFile(file: File, bufferSize: Int = 16384): Input[Array[Byte]] = {
    if (bufferSize < 256) throw new IllegalArgumentException(s"bufferSize must be >= 256 but was $bufferSize")
    val fileSize = file.length()
    if (fileSize > bufferSize) fromInputStream(new FileInputStream(file), bufferSize)
    else fromByteArray(Files.readAllBytes(file.toPath))
  }

}
