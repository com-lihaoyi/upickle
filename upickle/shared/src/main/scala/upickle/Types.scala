package upickle

import scala.{PartialFunction => PF}
import language.experimental.macros
import scala.annotation.implicitNotFound
import language.higherKinds
import acyclic.file
class ReaderPicker[M[_]]
class WriterPicker[M[_]]

/**
* Basic functionality to be able to read and write objects. Kept as a trait so
* other internal files can use it, while also mixing it into the `upickle`
* package to form the public API
*/
trait Types{ types =>
  /**
   * Classes that provides a mutable version of [[ReadWriter]], used to
   * allow serialization and deserialization of recursive data structure
   */
  object Knot {

    class RW[T](var _write: T => Js.Value, var _read: PF[Js.Value, T]) extends types.Reader[T] with types.Writer[T] {
      def read0 = _read

      def write0 = _write

      def copyFrom(rw: types.Reader[T] with types.Writer[T]) = {
        _write = rw.write
        _read = rw.read
      }
    }

    case class Reader[T](reader0: () => types.Reader[T]) extends types.Reader[T] {
      lazy val reader = reader0()
      def read0 = reader.read0
    }

    case class Writer[T](writer0: () => types.Writer[T]) extends types.Writer[T] {
      lazy val writer = writer0()
      def write0 = writer.write0
    }
  }


  /**
   * Helper object that makes it convenient to create instances of bother
   * [[Reader]] and [[Writer]] at the same time.
   */
  object ReadWriter {
    def apply[T](_write: T => Js.Value, _read: PF[Js.Value, T]): Writer[T] with Reader[T] = new Writer[T] with Reader[T]{
      def read0 = _read
      def write0 = _write
    }
  }

  type ReadWriter[T] = Reader[T] with Writer[T]
  /**
   * A typeclass that allows you to serialize a type [[T]] to JSON, and
   * eventually to a string
   */
  @implicitNotFound(
    "uPickle does not know how to write [${T}]s; define an implicit Writer[${T}] to teach it how"
  )
  trait Writer[T]{
    def write0: T => Js.Value
    final def write: T => Js.Value = {
      case null => Js.Null
      case t => write0(t)
    }
  }
  object Writer{

    /**
     * Helper class to make it convenient to create instances of [[Writer]]
     * from the equivalent function
     */
    def apply[T](_write: T => Js.Value): Writer[T] = new Writer[T]{
      val write0 = _write
    }

  }
  /**
   * A typeclass that allows you to deserialize a type [[T]] from JSON,
   * which can itself be read from a String
   */
  @implicitNotFound(
    "uPickle does not know how to read [${T}]s; define an implicit Reader[${T}] to teach it how"
  )
  trait Reader[T]{
    def read0: PF[Js.Value, T]

    private def readNull: PF[Js.Value, T] = {
      case Js.Null => null.asInstanceOf[T]
    }

    private def invalid: PF[Js.Value, T] = {
      case any => throw Invalid.Data(any, "name")
    }

    /*
     * This was originally equivalent to [[readNull orElse read0]].  However,
     * that limited flexibility because there was no way to pick off null
     * for some other reason (ie: deserializing an Option[String] as null and
     * a string).
     * 
     * This reverses the order.  However, in the original ordering read0 will throw
     * an exception when called with a value not in the domain.  In the new ordering
     * it won't throw that exception because isDefinedAt is called and prevents
     * the call to apply.  By adding it again at the end, the original behaviour of
     * throwing the exception is restored.
     */

    //final def read: PF[Js.Value, T] = readNull orElse read0
    final def read: PF[Js.Value, T] = read0 orElse readNull orElse read0


  }
  object Reader{
    /**
     * Helper class to make it convenient to create instances of [[Reader]]
     * from the equivalent function
     */
    def apply[T](_read: PF[Js.Value, T]): Reader[T] = new Reader[T]{
      def read0 = _read
    }
  }

  /**
   * Handy shorthands for Reader and Writer
   */
  object Aliases{
    type R[T] = Reader[T]
    val R = Reader

    type W[T] = Writer[T]
    val W = Writer

    type RW[T] = R[T] with W[T]
    val RW = ReadWriter
  }


  /**
   * Serialize an object of type [[T]] to a `String`
   */
  def write[T: Writer](expr: T, indent: Int = 0): String = json.write(writeJs(expr), indent)
  /**
   * Serialize an object of type [[T]] to a `Js.Value`
   */
  def writeJs[T: Writer](expr: T): Js.Value = implicitly[Writer[T]].write(expr)
  /**
   * Deserialize a `String` object of type [[T]]
   */
  def read[T: Reader](expr: String): T = readJs[T](json.read(expr))
  /**
   * Deserialize a `Js.Value` object of type [[T]]
   */
  def readJs[T: Reader](expr: Js.Value): T = implicitly[Reader[T]].read(expr)
}