package upickle
import acyclic.file
import scala.{PartialFunction => PF}

/**
 * A typeclass that allows you to serialize a type [[T]] to JSON, and
 * eventually to a string
 */
trait Writer[T]{def write: T => Js.Value}
/**
 * A typeclass that allows you to deserialize a type [[T]] from JSON,
 * which can itself be read from a String
 */
trait Reader[T]{def read: PF[Js.Value, T]}

/**
 * Helper class to make it convenient to create instances of [[Writer]]
 * from the equivalent function
 */
class WriterCls[T](val write: T => Js.Value) extends Writer[T]
/**
 * Helper class to make it convenient to create instances of [[Reader]]
 * from the equivalent function
 */
class ReaderCls[T](val read: PF[Js.Value, T]) extends Reader[T]

/**
 * Helper class that makes it convenient to create instances of bother
 * [[Reader]] and [[Writer]] at the same time.
 */
class ReadWriter[T](val write: T => Js.Value, val read: PF[Js.Value, T]) extends Writer[T] with Reader[T]

/**
 * A class that provides a mutable version of [[ReadWriter]], used to
 * allow serialization and deserialization of recursive data structure
 */
class RWKnot[T](var _write: T => Js.Value, var _read: PF[Js.Value, T]) extends Reader[T] with Writer[T]{
  def read = _read
  def write = _write
  def copyFrom(rw: ReadWriter[T]) = {
    _write = rw.write
    _read = rw.read
  }
}

