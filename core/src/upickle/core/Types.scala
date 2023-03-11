package upickle.core

import scala.language.experimental.macros
import scala.language.higherKinds
import scala.reflect.ClassTag

/**
* Basic functionality to be able to read and write objects. Kept as a trait so
* other internal files can use it, while also mixing it into the `upickle`
* package to form the public API1
*/
trait Types{ types =>

  /**
    * A combined [[Reader]] and [[Writer]], along with some utility methods.
    */
  trait ReadWriter[T] extends Reader[T] with Writer[T]{
    override def narrow[K] = this.asInstanceOf[ReadWriter[K]]
    def bimap[V](f: V => T, g: T => V): ReadWriter[V] = {
      new Visitor.MapReader[Any, T, V](ReadWriter.this) with ReadWriter[V]{
        def write0[Z](out: Visitor[_, Z], v: V) = {
          ReadWriter.this.write(out, f(v.asInstanceOf[V]))
        }

        override def mapNonNullsFunction(t: T) = g(t)
      }
    }
  }

  protected def joinReadWriter[T](implicit r0: Reader[T], w0: Writer[T]): ReadWriter[T]
  object ReadWriter{
    abstract class Delegate[T](other: Visitor[Any, T])
      extends Visitor.Delegate[Any, T](other) with ReadWriter[T]

    def merge[T](rws: ReadWriter[_ <: T]*): ReadWriter[T] = mergeReadWriters(rws: _*)

    implicit def join[T](implicit r0: Reader[T], w0: Writer[T]): ReadWriter[T] =
      joinReadWriter(r0, w0)
      
  }

  /**
    * A Reader that throws an error for all the visit methods which it does not define,
    * letting you only define the handlers you care about.
    */
  trait SimpleReader[T] extends Reader[T] with upickle.core.SimpleVisitor[Any, T]

  /**
    * Represents the ability to read a value of type [[T]].
    *
    * A thin wrapper around [[Visitor]], but needs to be it's own class in order
    * to make type inference automatically pick up it's implicit values.
    */
  trait Reader[T] extends upickle.core.Visitor[Any, T]{

    override def map[Z](f: T => Z): Reader[Z] = new Reader.MapReader[T, T, Z](Reader.this){
      def mapNonNullsFunction(v: T): Z = f(v)
    }
    override def mapNulls[Z](f: T => Z): Reader[Z] = new Reader.MapReader[T, T, Z](Reader.this){
      override def mapFunction(v: T): Z = f(v)
      def mapNonNullsFunction(v: T): Z = f(v)
    }

    def narrow[K <: T] = this.asInstanceOf[Reader[K]]
  }

  object Reader{
    class Delegate[T, V](delegatedReader: Visitor[T, V])
      extends Visitor.Delegate[T, V](delegatedReader) with Reader[V]{
      override def visitObject(length: Int, jsonableKeys: Boolean, index: Int) = {
        super.visitObject(length, jsonableKeys, index).asInstanceOf[ObjVisitor[Any, V]]
      }

      override def visitArray(length: Int, index: Int) = super.visitArray(length, index).asInstanceOf[ArrVisitor[Any, V]]
    }

    abstract class MapReader[-T, V, Z](delegatedReader: Visitor[T, V])
      extends Visitor.MapReader[T, V, Z](delegatedReader) with Reader[Z] {

      def mapNonNullsFunction(t: V): Z

      override def visitObject(length: Int, jsonableKeys: Boolean, index: Int) = {
        super.visitObject(length, jsonableKeys, index).asInstanceOf[ObjVisitor[Any, Z]]
      }

      override def visitArray(length: Int, index: Int) = super.visitArray(length, index).asInstanceOf[ArrVisitor[Any, Z]]
    }
    def merge[T](readers0: Reader[_ <: T]*): Reader[T] = mergeReaders(readers0: _*)
  }

  /**
    * Represents the ability to write a value of type [[T]].
    *
    * Generally nothing more than a way of applying the [[T]] to
    * a [[Visitor]], along with some utility methods
    */
  trait Writer[T] {
    /**
     * Whether or not the type being written can be used as a key in a JSON dictionary.
     * Opt-in, and only applicable to writers that write primitive types like
     * strings, booleans, numbers, etc..
     */
    def isJsonDictKey: Boolean = false
    def narrow[K] = this.asInstanceOf[Writer[K]]
    def transform[V](v: T, out: Visitor[_, V]) = write(out, v)
    def write0[V](out: Visitor[_, V], v: T): V
    def write[V](out: Visitor[_, V], v: T): V = {
      if (v == null) out.visitNull(-1)
      else write0(out, v)
    }
    def comapNulls[U](f: U => T) = new Writer.MapWriterNulls[U, T](this, f)
    def comap[U](f: U => T) = new Writer.MapWriter[U, T](this, f)
  }
  object Writer {

    class MapWriterNulls[U, T](src: Writer[T], f: U => T) extends Writer[U] {
      override def write[R](out: Visitor[_, R], v: U): R = src.write(out, f(v))
      def write0[R](out: Visitor[_, R], v: U): R = src.write(out, f(v))
    }
    class MapWriter[U, T](src: Writer[T], f: U => T) extends Writer[U] {
      def write0[R](out: Visitor[_, R], v: U): R = src.write(out, f(v))
    }
    def merge[T](writers: Writer[_ <: T]*): Writer[T] = mergeWriters(writers: _*)
  }


  protected def mergeReadWriters[T](rws: ReadWriter[_ <: T]*): ReadWriter[T]

  protected def mergeReaders[T](rws: Reader[_ <: T]*): Reader[T]

  protected def mergeWriters[T](rws: Writer[_ <: T]*): Writer[T]
}
