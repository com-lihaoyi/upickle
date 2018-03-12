package upickle

import scala.{PartialFunction => PF}
import language.experimental.macros
import scala.annotation.implicitNotFound
import language.higherKinds
import acyclic.file
import jawn.{Facade, RawFContext, RawFacade}

import scala.collection.mutable
import scala.reflect.ClassTag

/**
* Basic functionality to be able to read and write objects. Kept as a trait so
* other internal files can use it, while also mixing it into the `upickle`
* package to form the public API1
*/
trait Types{ types =>
  type ReadWriter[T] = Reader[T] with Writer[T]
  type TaggedReadWriter[T] = TaggedReader[T] with TaggedWriter[T]
  type TaggedReader[T] <: TaggedReader0[T]
  trait TaggedReader0[T] extends Reader[T] {
    def tags: Seq[String]
    def readers: Seq[Reader[T]]
  }
  def newTaggedReader[T](tags0: Seq[String], readers0: Seq[Reader[T]]): TaggedReader[T]
  trait TaggedWriter[T] extends Writer[T]{
    def tags: Seq[String]
  }
  object ReadWriter{
    implicit class Mergable[T, K <: T](w0: ReadWriter[K])
                                      (implicit val ct: ClassTag[K]){
      val w = new TaggedReader0[K] with TaggedWriter[K]{
        def tags = w0.asInstanceOf[TaggedReadWriter[K]].tags

        def readers = w0.asInstanceOf[TaggedReadWriter[K]].readers

        def write[R](out: Facade[R], v: K): R = w0.write(out, v)
      }
    }
    def merge[T](rws: Mergable[T, _]*): TaggedReadWriter[T] = {
      joinTagged(
        Reader.merge[T](
          rws.map(x =>
            new Reader.Mergable[T, T](
              x.w.asInstanceOf[TaggedReadWriter[T]]
            )(x.ct.asInstanceOf[ClassTag[T]])
          ):_*
        ),
        Writer.merge[T](
          rws.map(x =>
            new Writer.Mergable[T, T](
              x.w.asInstanceOf[TaggedReadWriter[T]]
            )(x.ct.asInstanceOf[ClassTag[T]])
          ):_*
        )
      )
    }

    def join[T: Reader: Writer]: ReadWriter[T] = new Reader[T] with Writer[T] with BaseReader.Delegate[Any, T]{
      def delegatedReader = implicitly[Reader[T]]

      def write[R](out: Facade[R], v: T): R = {
        implicitly[Writer[T]].write(out, v)
      }
    }
  }
  def joinTagged[T: TaggedReader: TaggedWriter]: TaggedReadWriter[T]
  object Reader{
    implicit class Mergable[T, K <: T](r0: Reader[K])(implicit val ct: ClassTag[K]){
      val r = newTaggedReader[K](
        r0.asInstanceOf[TaggedReader0[K]].tags,
        r0.asInstanceOf[TaggedReader0[K]].readers
      )
    }
    def merge[T](readers0: Mergable[T, _]*) = newTaggedReader[T](
      readers0.flatMap(_.r.tags),
      readers0.flatMap(_.r.readers.asInstanceOf[Seq[Reader[T]]])
    )
  }
  type Reader[T] = BaseReader[Any, T]
  trait BaseReader[-T, V] extends jawn.RawFacade[T, V] {
    def narrow[K <: V] = this.asInstanceOf[BaseReader[T, K]]
    def jnull(index: Int): V = null.asInstanceOf[V]
    def jtrue(index: Int): V =  throw new Exception(index.toString)
    def jfalse(index: Int): V = throw new Exception(index.toString)

    def jstring(s: CharSequence, index: Int): V = throw new Exception(index.toString)
    def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): V = throw new Exception(index.toString)

    def objectContext(index: Int): jawn.RawFContext[T, V] = throw new Exception(index.toString)
    def arrayContext(index: Int): jawn.RawFContext[T, V] = throw new Exception(index.toString)
    def map[Z](f: V => Z) = new BaseReader.MapReader[T, V, Z](this, f)
    def singleContext(index: Int): jawn.RawFContext[T, V] = new RawFContext[T, V] {
      var res: V = _

      def facade = BaseReader.this

      def visitKey(s: CharSequence, index: Int): Unit = ???

      def add(v: T, index: Int): Unit = {
        res = v.asInstanceOf[V]
      }

      def finish(index: Int) = res

      def isObj = false
    }
  }

  object BaseReader {
    trait Delegate[T, V] extends BaseReader[T, V]{
      def delegatedReader: BaseReader[T, V]

      override def jnull(index: Int) = delegatedReader.jnull(index)
      override def jtrue(index: Int) = delegatedReader.jtrue(index)
      override def jfalse(index: Int) = delegatedReader.jfalse(index)

      override def jstring(s: CharSequence, index: Int) = delegatedReader.jstring(s, index)
      override def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
        delegatedReader.jnum(s, decIndex, expIndex, index)
      }

      override def objectContext(index: Int) = delegatedReader.objectContext(index)
      override def arrayContext(index: Int) = delegatedReader.arrayContext(index)
      override def singleContext(index: Int) = delegatedReader.singleContext(index)
    }
    class MapReader[-T, V, Z](src: BaseReader[T, V], f: V => Z) extends BaseReader[T, Z] {
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
    def write[V](out: jawn.Facade[V], v: T): V
    def comap[U](f: U => T) = new Writer.MapWriter[U, T](this, f)
  }
  object Writer {

    class MapWriter[U, T](src: Writer[T], f: U => T) extends Writer[U] {
      def write[R](out: jawn.Facade[R], v: U): R =
        src.write(out, if(v == null) null.asInstanceOf[T] else f(v))
    }
    implicit class Mergable[T, K <: T](w0: Writer[K])(implicit val ct: ClassTag[K]){
      val w = new TaggedWriter[K]{
        def tags = w0.asInstanceOf[TaggedWriter[K]].tags

        def write[V](out: Facade[V], v: K): V = w0.write(out, v)
      }
    }
    def merge[T](writers: Mergable[T, _]*) = new TaggedWriter[T] {
      def tags = writers.flatMap(_.w.tags)
      def write[R](out: Facade[R], v: T): R = {
        val w = writers.find(_.ct.runtimeClass.isInstance(v)).get.w
        w.asInstanceOf[Writer[Any]].write(out, v)
      }
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