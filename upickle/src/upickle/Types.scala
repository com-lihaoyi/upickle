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
  trait TaggedReader[T] extends Reader[T]{
    def tags: Seq[String]
    def readers: Seq[Reader[T]]

    override def jstring(cs: CharSequence, index: Int) = cs.toString.asInstanceOf[T]
    override def arrayContext(index: Int) = new RawFContext[Any, T] {
      var typeName: String = null
      var delegate: Reader[_] = null
      var delegateCtx: RawFContext[_, T] = null

      var res: T = _
      def visitKey(s: CharSequence, index: Int): Unit = ???
      def facade =
        if (typeName == null) TaggedReader.this.asInstanceOf[RawFacade[Any]]
        else delegate.asInstanceOf[RawFacade[Any]]

      def add(v: Any, index: Int): Unit = {
        if (typeName == null){
          typeName = v.toString
          delegate = readers(tags.indexOf(typeName))
        }else{
          res = v.asInstanceOf[T]
        }
      }

      def finish(index: Int) = {
        res
      }

      def isObj = false
    }
  }
  trait TaggedWriter[T] extends Writer[T]{
    def tags: Seq[String]
  }
  object ReadWriter{
    implicit class Mergable[T, K <: T](val w0: ReadWriter[K])
                                      (implicit val ct: ClassTag[K]){
      def w = w0.asInstanceOf[TaggedReadWriter[K]]
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

    def join[T: Reader: Writer]: ReadWriter[T] = new Reader[T] with Writer[T] {
      override def jnull(index: Int) = implicitly[Reader[T]].jnull(index)
      override def jtrue(index: Int) = implicitly[Reader[T]].jtrue(index)
      override def jfalse(index: Int) = implicitly[Reader[T]].jfalse(index)

      override def jstring(s: CharSequence, index: Int) = implicitly[Reader[T]].jstring(s, index)
      override def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
        implicitly[Reader[T]].jnum(s, decIndex, expIndex, index)
      }

      override def objectContext(index: Int) = implicitly[Reader[T]].objectContext(index)
      override def arrayContext(index: Int) = implicitly[Reader[T]].arrayContext(index)
      override def singleContext(index: Int) = implicitly[Reader[T]].singleContext(index)

      def write(out: Facade[Unit], v: T): Unit = {
        implicitly[Writer[T]].write(out, v)
      }
    }
    def joinTagged[T: TaggedReader: TaggedWriter]: TaggedReadWriter[T] = new TaggedReader[T] with TaggedWriter[T] {
      def tags = implicitly[TaggedReader[T]].tags
      def readers = implicitly[TaggedReader[T]].readers
      override def jnull(index: Int) = implicitly[Reader[T]].jnull(index)
      override def jtrue(index: Int) = implicitly[Reader[T]].jtrue(index)
      override def jfalse(index: Int) = implicitly[Reader[T]].jfalse(index)

      override def jstring(s: CharSequence, index: Int) = implicitly[Reader[T]].jstring(s, index)
      override def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
        implicitly[Reader[T]].jnum(s, decIndex, expIndex, index)
      }

      override def objectContext(index: Int) = implicitly[Reader[T]].objectContext(index)
      override def arrayContext(index: Int) = implicitly[Reader[T]].arrayContext(index)
      override def singleContext(index: Int) = implicitly[Reader[T]].singleContext(index)

      def write(out: Facade[Unit], v: T): Unit = {
        implicitly[Writer[T]].write(out, v)
      }
    }
  }
  object Reader{
    implicit class Mergable[T, K <: T](val r0: Reader[K])(implicit val ct: ClassTag[K]){
      def r = r0.asInstanceOf[TaggedReader[K]]
    }
    def merge[T](readers0: Mergable[T, _]*) = new TaggedReader[T]{ outer =>
      val tags: Seq[String] = readers0.flatMap(_.r.tags)
      val readers: Seq[Reader[T]] = readers0.flatMap(_.r.readers.asInstanceOf[Seq[Reader[T]]])
    }
  }
  type Reader[T] = BaseReader[Any, T]
  trait BaseReader[T, V] extends jawn.RawFacade[V] {
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
    implicit class Mergable[T, K <: T](val w0: Writer[K])(implicit val ct: ClassTag[K]){
      def w = w0.asInstanceOf[TaggedWriter[T]]
    }
    def merge[T](writers: Mergable[T, _]*) = new TaggedWriter[T] {
      def tags = writers.flatMap(_.w.tags)
      def write(out: Facade[Unit], v: T): Unit = {
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