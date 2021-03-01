/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ujson.borer.output

import java.io.{BufferedOutputStream, File, FileOutputStream}

import ujson.borer.Output.ToValueProvider

trait ToFileOutput { this: ToOutputStreamOutput =>

  implicit object ToFileProvider extends ToValueProvider[File] {
    type Out = ToFile
    def apply(file: File, bufferSize: Int, allowBufferCaching: Boolean) = new ToFile(file, bufferSize)
  }

  /**
    * Default, mutable implementation for serializing to a given [[File]].
    */
  final class ToFile(file: File, bufferSize: Int)
      extends ToOutputStreamBase(new BufferedOutputStream(new FileOutputStream(file), bufferSize), bufferSize) {
    type Self   = ToFile
    type Result = File

    def result(): File = {
      outputStream.close()
      file
    }
  }
}
