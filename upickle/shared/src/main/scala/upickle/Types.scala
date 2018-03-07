package upickle

import scala.{PartialFunction => PF}
import language.experimental.macros
import scala.annotation.implicitNotFound
import language.higherKinds
import acyclic.file
import jawn.RawFContext

import scala.reflect.ClassTag

/**
* Basic functionality to be able to read and write objects. Kept as a trait so
* other internal files can use it, while also mixing it into the `upickle`
* package to form the public API1
*/
trait Types{ types =>
  type Reader[T] = BaseReader[Any, T]
  trait BaseReader[T, V] extends jawn.RawFacade[V] {
    var empty: V = _
    def jnull(index: Int): V = empty
    def jtrue(index: Int): V =  throw new Exception(index.toString)
    def jfalse(index: Int): V = throw new Exception(index.toString)

    def jstring(s: CharSequence, index: Int): V = throw new Exception(index.toString)
    def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): V = throw new Exception(index.toString)

    def objectContext(index: Int): jawn.RawFContext[T, V] = throw new Exception(index.toString)
    def arrayContext(index: Int): jawn.RawFContext[T, V] = throw new Exception(index.toString)
    def map[Z](f: V => Z) = new BaseReader.MapReader[T, V, Z](this, f)
    def singleContext(index: Int): jawn.RawFContext[T, V] = new RawFContext[T, V] {
      var res: V = _

      def facade = BaseReader.this.asInstanceOf[jawn.RawFacade[T]]

      def visitKey(s: CharSequence, index: Int): Unit = ???

      def add(v: T, index: Int): Unit = {
        res = v.asInstanceOf[V]
      }

      def finish(index: Int) = res

      def isObj = false
    }
  }

  object BaseReader {
    class MapReader[T, V, Z](src: BaseReader[T, V], f: V => Z) extends BaseReader[T, Z] {
      def f1(v: V): Z = {
        if(v == null) null.asInstanceOf[Z] else f(v)
      }
      override def jfalse(index: Int) = f1(src.jfalse(index))
      override def jnull(index: Int) = f1(src.jnull(index))
      override def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
        f1(src.jnum(s, decIndex, expIndex, index))
      }
      override def jstring(s: CharSequence, index: Int) = {
        f1(src.jstring(s, index))
      }
      override def jtrue(index: Int) = f(src.jtrue(index))

      override def objectContext(index: Int): jawn.RawFContext[T, Z] = {
        new MapFContext[T, V, Z](src.objectContext(index), f)
      }
      override def arrayContext(index: Int): jawn.RawFContext[T, Z] = {
        new MapFContext[T, V, Z](src.arrayContext(index), f)
      }

      // We do not override the singleContext with a MapFContext, because
      // unlike array/object-Contexts, the value being returned by the `finish`
      // of singleContext is the same as the value being `add`ed to it. That
      // value has already been transformed by `f` when it was added, and so
      // does not need to be transformed again by the MapFContext
      //
      // override def singleContext(index: Int): jawn.RawFContext[T, Z] = {
      //   new MapFContext[T, V, Z](src.singleContext(index), f)
      // }
    }

    class MapFContext[T, V, Z](src: jawn.RawFContext[T, V],
                               f: V => Z) extends jawn.RawFContext[T, Z]{
      def facade = src.facade

      def visitKey(s: CharSequence, index: Int): Unit = src.visitKey(s, index)

      def add(v: T, index: Int): Unit = src.add(v, index)

      def finish(index: Int) = f(src.finish(index))

      def isObj = src.isObj
    }
  }
  trait Writer[T]{
    def write(out: jawn.Facade[Unit], v: T): Unit
    def comap[U](f: U => T) = new Writer.MapWriter[U, T](this, f)
  }
  object Writer {

    class MapWriter[U, T](src: Writer[T], f: U => T) extends Writer[U] {
      def write(out: jawn.Facade[Unit], v: U) =
        src.write(out, if(v == null) null.asInstanceOf[T] else f(v))
    }

  }


//  /**
//   * Serialize an object of type [[T]] to a `String`
//   */
//  def write[T: Writer](expr: T, indent: Int = 0): String = json.write(writeJs(expr), indent)
//  /**
//   * Serialize an object of type [[T]] to a `Js.Value`
//   */
//  def writeJs[T: Writer](expr: T): Js.Value = implicitly[Writer[T]].write(expr)
//  /**
//   * Deserialize a `String` object of type [[T]]
//   */
//  def read[T: Reader](expr: String): T = readJs[T](json.read(expr))
//  /**
//   * Deserialize a `Js.Value` object of type [[T]]
//   */
//  def readJs[T: Reader](expr: Js.Value): T = implicitly[Reader[T]].read(expr)
}