package ujson.borer

import scala.annotation.unchecked.uncheckedVariance


sealed abstract class Error[+IO](private var _io: IO @uncheckedVariance, msg: String, cause: Throwable = null)
  extends RuntimeException(msg, cause) {

  final override def getMessage = s"$msg (${_io})"

  final def io: IO = _io

  private[borer] def withPosOf(reader: Reader): Error[Input.Position] = {
    val thiz = this.asInstanceOf[Error[Input.Position]]
    if (thiz._io.asInstanceOf[AnyRef] eq null) thiz._io = reader.position
    thiz
  }

  private[borer] def withOut(out: Output): Error[Output] = {
    val thiz = this.asInstanceOf[Error[Output]]
    if (thiz._io eq null) thiz._io = out
    thiz
  }
}

object Error {

  final class UnexpectedEndOfInput[IO](io: IO, expected: String)
    extends Error[IO](io, s"Expected $expected but got end of input")

  final class InvalidInputData[IO](io: IO, msg: String) extends Error[IO](io, msg) {
    def this(io: IO, expected: String, actual: String) = this(io, s"Expected $expected but got $actual")
  }

  final class ValidationFailure[IO](io: IO, msg: String) extends Error[IO](io, msg)

  final class Unsupported[IO](io: IO, msg: String) extends Error[IO](io, msg)

  final class Overflow[IO](io: IO, msg: String) extends Error[IO](io, msg)

  final class General[IO](io: IO, cause: Throwable) extends Error[IO](io, cause.toString, cause)
}