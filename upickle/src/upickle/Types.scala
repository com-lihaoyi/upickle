package upickle

import language.experimental.macros
import language.higherKinds
import upickle.jawn.{Facade, RawFContext}

import scala.reflect.ClassTag

/**
* Basic functionality to be able to read and write objects. Kept as a trait so
* other internal files can use it, while also mixing it into the `upickle`
* package to form the public API1
*/
trait Types{ types =>
  type ReadWriter[T] = Reader[T] with Writer[T]

  def taggedArrayContext[T](taggedReader: TaggedReader[T], index: Int): RawFContext[Any, T] = throw new Exception(index.toString)
  def taggedObjectContext[T](taggedReader: TaggedReader[T], index: Int): RawFContext[Any, T] = throw new Exception(index.toString)
  def taggedWrite[T, R](w: Writer[T], tag: String, out: Facade[R], v: T): R
  sealed trait TaggedReader[T] extends Reader[T]{
    def findReader(s: String): Option[Reader[T]]

    override def arrayContext(index: Int) = taggedArrayContext(this, index)
    override def objectContext(index: Int) = taggedObjectContext(this, index)
  }
  object TaggedReader{
    case class Leaf[T](tag: String, r: Reader[T]) extends TaggedReader[T]{
      def findReader(s: String) = if (s == tag) Some(r) else None
    }
    case class Node[T](rs: TaggedReader[_ <: T]*) extends TaggedReader[T]{
      def findReader(s: String) = rs.map(_.findReader(s).asInstanceOf[Option[Reader[T]]]).collectFirst{case Some(x) => x}
    }
  }

  trait TaggedWriter[T] extends Writer[T]{
    def findWriter(v: Any): Option[(String, Writer[T])]
    def write[R](out: Facade[R], v: T): R = {
      val (tag, w) = findWriter(v).get
      taggedWrite(w, tag, out, v)

    }
  }
  object TaggedWriter{
    case class Leaf[T](tag: String, r: Writer[T]) extends TaggedWriter[T]{
      def findWriter(v: Any) = {
        if (v.getClass.getName.replace('$', '.').stripSuffix(".") == tag) Some(tag -> r)
        else None
      }
    }
    case class Node[T](rs: TaggedWriter[_ <: T]*) extends TaggedWriter[T]{
      assert(rs.forall(_.isInstanceOf[TaggedWriter[_ <: T]]))
      def findWriter(v: Any) = {
        rs.map(_.findWriter(v)).collectFirst{case Some(x: (String, Writer[T])) => x}
      }
    }
  }

  sealed trait TaggedReadWriter[T] extends TaggedReader[T] with TaggedWriter[T]{

    override def arrayContext(index: Int) = taggedArrayContext(this, index)
    override def objectContext(index: Int) = taggedObjectContext(this, index)

  }
  object TaggedReadWriter{
    case class Leaf[T](tag: String, r: ReadWriter[T]) extends TaggedReadWriter[T]{
      def findReader(s: String) = if (s == tag) Some(r) else None
      def findWriter(v: Any) = {
        if (v.getClass.getName.replace('$', '.').stripSuffix(".") == tag) Some(tag -> r)
        else None
      }
    }
    case class Node[T](rs: TaggedReadWriter[_ <: T]*) extends TaggedReadWriter[T]{
      def findReader(s: String) = rs.map(_.findReader(s).asInstanceOf[Option[Reader[T]]]).collectFirst{case Some(x) => x}
      def findWriter(v: Any) = {
        rs.map(_.findWriter(v)).collectFirst{case Some(x: (String, Writer[T])) => x}
      }
    }
  }

  object ReadWriter{

    def merge[T](rws: ReadWriter[_ <: T]*): TaggedReadWriter[T] = {
      TaggedReadWriter.Node(rws.asInstanceOf[Seq[TaggedReadWriter[T]]]:_*)
    }

    def join[T: Reader: Writer]: ReadWriter[T] = new Reader[T] with Writer[T] with BaseReader.Delegate[Any, T]{
      def delegatedReader = implicitly[Reader[T]]

      def write[R](out: Facade[R], v: T): R = {
        implicitly[Writer[T]].write(out, v)
      }
    }
  }

  object Reader{

    def merge[T](readers0: Reader[_ <: T]*) = TaggedReader.Node(readers0.asInstanceOf[Seq[TaggedReader[T]]]:_*)
  }
  type Reader[T] = BaseReader[Any, T]
  trait BaseReader[-T, V] extends upickle.jawn.RawFacade[T, V] {
    def narrow[K <: V] = this.asInstanceOf[BaseReader[T, K]]
    def jnull(index: Int): V = null.asInstanceOf[V]
    def jtrue(index: Int): V =  throw new Exception(index.toString)
    def jfalse(index: Int): V = throw new Exception(index.toString)

    def jstring(s: CharSequence, index: Int): V = throw new Exception(index.toString)
    def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): V = throw new Exception(index.toString)

    def objectContext(index: Int): upickle.jawn.RawFContext[T, V] = throw new Exception(index.toString)
    def arrayContext(index: Int): upickle.jawn.RawFContext[T, V] = throw new Exception(index.toString)
    def map[Z](f: V => Z) = new BaseReader.MapReader[T, V, Z](this, f)
    def singleContext(index: Int): upickle.jawn.RawFContext[T, V] = new RawFContext[T, V] {
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

      override def objectContext(index: Int): upickle.jawn.RawFContext[T, Z] = {
        new MapFContext[T, V, Z](src.objectContext(index), f)
      }
      override def arrayContext(index: Int): upickle.jawn.RawFContext[T, Z] = {
        new MapFContext[T, V, Z](src.arrayContext(index), f)
      }

      // We do not override the singleContext with a MapFContext, because
      // unlike array/object-Contexts, the value being returned by the `finish`
      // of singleContext is the same as the value being `add`ed to it. That
      // value has already been transformed by `f` when it was added, and so
      // does not need to be transformed again by the MapFContext
      //
      // override def singleContext(index: Int): upickle.jawn.RawFContext[T, Z] = {
      //   new MapFContext[T, V, Z](src.singleContext(index), f)
      // }
    }

    class MapFContext[T, V, Z](src: upickle.jawn.RawFContext[T, V],
                               f: V => Z) extends upickle.jawn.RawFContext[T, Z]{
      def facade = src.facade

      def visitKey(s: CharSequence, index: Int): Unit = src.visitKey(s, index)

      def add(v: T, index: Int): Unit = src.add(v, index)

      def finish(index: Int) = f(src.finish(index))

      def isObj = src.isObj
    }
  }
  trait Writer[T]{
    def write[V](out: upickle.jawn.Facade[V], v: T): V
    def comap[U](f: U => T) = new Writer.MapWriter[U, T](this, f)
  }
  object Writer {

    class MapWriter[U, T](src: Writer[T], f: U => T) extends Writer[U] {
      def write[R](out: upickle.jawn.Facade[R], v: U): R =
        src.write(out, if(v == null) null.asInstanceOf[T] else f(v))
    }
    def merge[T](writers: Writer[_ <: T]*) = TaggedWriter.Node(writers.asInstanceOf[Seq[TaggedWriter[T]]]:_*)
  }
}