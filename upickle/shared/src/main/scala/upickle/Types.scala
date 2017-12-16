package upickle

import scala.{PartialFunction => PF}
import language.experimental.macros
import scala.annotation.implicitNotFound
import language.higherKinds
import acyclic.file

import scala.reflect.ClassTag

/**
* Basic functionality to be able to read and write objects. Kept as a trait so
* other internal files can use it, while also mixing it into the `upickle`
* package to form the public API1
*/
trait Types{ types =>
  /**
   * Helper object that makes it convenient to create instances of bother
   * [[Reader]] and [[Writer]] at the same time.
   */
  object ReadWriter {
    def apply[T](_write: T => Js.Value, _read: PF[Js.Value, T])
                (implicit src: sourcecode.Enclosing)
                : Writer[T] with Reader[T] = new Writer[T] with Reader[T]{
      val read0 = _read
      val write0 = _write
      override def toString = src.value
    }

    implicit class Mergable[T, K <: T](val w: Writer[K] with Reader[K])
                                      (implicit val ct: ClassTag[K]){
      def tryRead(v: Any) = v match{
        case t: K => Some(w.write(t))
        case _ => None
      }
    }
    def merge[T](rws: Mergable[T, _]*) = new Writer[T] with Reader[T]{
      def write0 = Writer.merge0[T](rws.map(_.tryRead _):_*)

      def read0 = Reader.merge0[T](
        rws.map(_.w.read.asInstanceOf[PF[Js.Value, T]]):_*
      )
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
    final val write: T => Js.Value = {
      case null => Js.Null
      case t => write0(t)
    }

  }
  object Writer{
    implicit class Mergable[T, K <: T](val w: Writer[K])(implicit val ct: ClassTag[K]){
      def tryRead(v: Any) = v match{
        case t: K => Some(w.write(t))
        case _ => None
      }
    }

    def merge0[T](tryReads: (T => Option[Js.Value])*) = {
      (v: T) => {
        val iter = tryReads.iterator.flatMap(_(v))
        if (iter.hasNext) iter.next()
        else {
          throw new Exception("Writer unable to write object " + v)
        }

      }
    }
    def merge[T](writers: Mergable[T, _]*) = {
      Writer[T](merge0(writers.map(_.tryRead _):_*))
    }
    /**
     * Helper class to make it convenient to create instances of [[Writer]]
     * from the equivalent function
     */
    def apply[T](_write: T => Js.Value)
                (implicit src: sourcecode.Enclosing): Writer[T] = new Writer[T]{
      val write0 = _write
      override def toString = src.value
    }

  }
  /**
   * A typeclass that allows you to deserialize a type [[T]] from JSON,
   * which can itself be read from a String
   */
  @implicitNotFound(
    "uPickle does not know how to read [${T}]s; define an implicit Reader[${T}] to teach it how"
  )
  trait Reader[T] {

    def read0: PF[Js.Value, T]

    private val readNull: PF[Js.Value, T] = {
      case Js.Null => null.asInstanceOf[T]
    }

    final val read: PF[Js.Value, T] = new PartialFunction[Js.Value, T] {
      def isDefinedAt(x: Js.Value) = x == Js.Null || read0.isDefinedAt(x)

      /**
        * Do this `isDefinedAt` dance to make sure we throw the correct error
        * message (that of `read0` instead of `readNull` in the case where someone
        * calls `read.apply` on some invalid value
        */
      def apply(v1: Js.Value): T = {
        if (!this.isDefinedAt(v1)) read0(v1)
        else read0.applyOrElse(v1, readNull)
      }
    }
  }

  object Reader{
    implicit class Mergable[T, K <: T](val r: Reader[K])
    def merge0[T](readers: PF[Js.Value, T]*) = {
      readers.iterator.reduceLeft(_.orElse(_))
    }
    def merge[T](readers: Mergable[T, _]*) = {
      Reader[T](merge0[T](readers.map(_.r.read.asInstanceOf[PF[Js.Value, T]]):_*))
    }
    /**
     * Helper class to make it convenient to create instances of [[Reader]]
     * from the equivalent function
     */
    def apply[T](_read: PF[Js.Value, T])
                (implicit src: sourcecode.Enclosing): Reader[T] = new Reader[T]{
      val read0 = _read
      override def toString = src.value
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