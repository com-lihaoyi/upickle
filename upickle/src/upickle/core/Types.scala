package upickle
package core

import upickle.jawn._

import scala.language.experimental.macros
import scala.language.higherKinds
import scala.reflect.ClassTag

/**
* Basic functionality to be able to read and write objects. Kept as a trait so
* other internal files can use it, while also mixing it into the `upickle`
* package to form the public API1
*/
trait Types{ types =>
  type ReadWriter[T] = Reader[T] with Writer[T]

  object ReadWriter{

    def merge[T](rws: ReadWriter[_ <: T]*): TaggedReadWriter[T] = {
      new TaggedReadWriter.Node(rws.asInstanceOf[Seq[TaggedReadWriter[T]]]:_*)
    }

    def join[T: Reader: Writer]: ReadWriter[T] = new Reader[T] with Writer[T] with BaseReader.Delegate[Any, T]{
      override def narrow[K <: T] = this.asInstanceOf[ReadWriter[K]]
      def delegatedReader = implicitly[Reader[T]]

      def write[R](out: Visitor[_, R], v: T): R = {
        implicitly[Writer[T]].write(out, v)
      }
    }
  }

  object Reader{

    def merge[T](readers0: Reader[_ <: T]*) = {
      new TaggedReader.Node(readers0.asInstanceOf[Seq[TaggedReader[T]]]:_*)
    }
  }
  type Reader[T] = BaseReader[Any, T]
  trait BaseReader[-T, V] extends upickle.jawn.Visitor[T, V] {
    def expectedMsg = ""
    def narrow[K <: V] = this.asInstanceOf[BaseReader[T, K]]
    def jnull(index: Int): V = null.asInstanceOf[V]
    def jtrue(index: Int): V =  throw new AbortJsonProcessingException(expectedMsg + " got boolean")
    def jfalse(index: Int): V = throw new AbortJsonProcessingException(expectedMsg + " got boolean")

    def jstring(s: CharSequence, index: Int): V = {
      throw new AbortJsonProcessingException(expectedMsg + " got string")
    }
    def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): V = {
      throw new AbortJsonProcessingException(expectedMsg + " got number")
    }

    def objectContext(index: Int): upickle.jawn.ObjVisitor[T, V] = {
      throw new AbortJsonProcessingException(expectedMsg + " got dictionary")
    }
    def arrayContext(index: Int): upickle.jawn.ArrVisitor[T, V] = {
      throw new AbortJsonProcessingException(expectedMsg + " got sequence")
    }
    def map[Z](f: V => Z) = new BaseReader.MapReader[T, V, Z](this, f)
    def mapNulls[Z](f: V => Z) = new BaseReader.MapReaderNullable[T, V, Z](this, f)
    def singleContext(index: Int) = new ObjVisitor[T, V] {
      var res: V = _

      def subVisitor = BaseReader.this

      def visitKey(s: CharSequence, index: Int): Unit = ???

      def add(v: T, index: Int): Unit = {
        res = v.asInstanceOf[V]
      }

      def finish(index: Int) = res
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
    class MapReaderNullable[-T, V, Z](src: BaseReader[T, V], f: V => Z) extends MapReader[T, V, Z](src, f ){
      override def f1(v: V): Z = f(v)
    }
    class MapReader[-T, V, Z](src: BaseReader[T, V], f: V => Z) extends BaseReader[T, Z] {
      def f1(v: V): Z = if(v == null) null.asInstanceOf[Z] else f(v)

      override def jfalse(index: Int) = f1(src.jfalse(index))
      override def jnull(index: Int) = f1(src.jnull(index))
      override def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
        f1(src.jnum(s, decIndex, expIndex, index))
      }
      override def jstring(s: CharSequence, index: Int) = {
        f1(src.jstring(s, index))
      }
      override def jtrue(index: Int) = f(src.jtrue(index))

      override def objectContext(index: Int): upickle.jawn.ObjVisitor[T, Z] = {
        new MapObjContext[T, V, Z](src.objectContext(index), f)
      }
      override def arrayContext(index: Int): upickle.jawn.ArrVisitor[T, Z] = {
        new MapArrContext[T, V, Z](src.arrayContext(index), f)
      }

      // We do not override the singleContext with a MapFContext, because
      // unlike array/object-Contexts, the value being returned by the `finish`
      // of singleContext is the same as the value being `add`ed to it. That
      // value has already been transformed by `f` when it was added, and so
      // does not need to be transformed again by the MapFContext
      //
      // override def singleContext(index: Int): upickle.jawn.ObjArrVisitor[T, Z] = {
      //   new MapFContext[T, V, Z](src.singleContext(index), f)
      // }
    }

    class MapArrContext[T, V, Z](src: upickle.jawn.ArrVisitor[T, V],
                               f: V => Z) extends upickle.jawn.ArrVisitor[T, Z]{
      def subVisitor = src.subVisitor

      def add(v: T, index: Int): Unit = src.add(v, index)

      def finish(index: Int) = f(src.finish(index))
    }

    class MapObjContext[T, V, Z](src: upickle.jawn.ObjVisitor[T, V],
                               f: V => Z) extends upickle.jawn.ObjVisitor[T, Z]{
      def subVisitor = src.subVisitor

      def visitKey(s: CharSequence, index: Int) = src.visitKey(s, index)

      def add(v: T, index: Int): Unit = src.add(v, index)

      def finish(index: Int) = f(src.finish(index))
    }
  }
  trait Writer[T]{
    def narrow[K <: T] = this.asInstanceOf[Writer[K]]
    def write[V](out: upickle.jawn.Visitor[_, V], v: T): V
    def comapNulls[U](f: U => T) = new Writer.MapWriterNulls[U, T](this, f)
    def comap[U](f: U => T) = new Writer.MapWriter[U, T](this, f)
  }
  object Writer {

    class MapWriterNulls[U, T](src: Writer[T], f: U => T) extends Writer[U] {
      def write[R](out: upickle.jawn.Visitor[_, R], v: U): R = {
        src.write(out, f(v))
      }
    }
    class MapWriter[U, T](src: Writer[T], f: U => T) extends Writer[U] {
      def write[R](out: upickle.jawn.Visitor[_, R], v: U): R = {
        if (v == null) out.jnull(-1)
        else src.write(out, f(v))
      }
    }
    def merge[T](writers: Writer[_ <: T]*) = {
      new TaggedWriter.Node(writers.asInstanceOf[Seq[TaggedWriter[T]]]:_*)
    }
  }

  class TupleNWriter[V](val writers: Array[Writer[_]], val f: V => Array[Any]) extends Writer[V]{
    def write[R](out: upickle.jawn.Visitor[_, R], v: V): R = {
      if (v == null) out.jnull(-1)
      else{
        val ctx = out.arrayContext()
        val vs = f(v)
        var i = 0
        while(i < writers.length){
          ctx.add(
            writers(i).asInstanceOf[Writer[Any]].write(
              out.asInstanceOf[Visitor[Any, Nothing]],
              vs(i)
            ),
            -1
          )
          i += 1
        }
        ctx.finish(-1)
      }
    }
  }

  class TupleNReader[V](val readers: Array[Reader[_]], val f: Array[Any] => V) extends Reader[V]{

    override def expectedMsg = "expected sequence"
    override def arrayContext(index: Int) = new upickle.jawn.ArrVisitor[Any, V] {
      val b = new Array[Any](readers.length)
      var facadesIndex = 0

      var start = facadesIndex
      def add(v: Any, index: Int): Unit = {
        b(facadesIndex % readers.length) = v
        facadesIndex = facadesIndex + 1
      }

      def finish(index: Int) = {
        val lengthSoFar = facadesIndex - start
        if (lengthSoFar != readers.length) {
          throw new AbortJsonProcessingException(
            "expected " + readers.length + " items in sequence, found " + lengthSoFar
          )
        }
        start = facadesIndex

        f(b)

      }

      def subVisitor = readers(facadesIndex % readers.length)
    }
  }

  abstract class CaseR[V](val argCount: Int) extends Reader[V]{
    override def expectedMsg = "expected dictionary"
    trait CaseObjectContext extends upickle.jawn.ObjVisitor[Any, V]{
      val aggregated = new Array[Any](argCount)
      val found = new Array[Boolean](argCount)
      var currentIndex = -1
      var count = 0
      def add(v: Any, index: Int): Unit = {
        if (currentIndex != -1 && !found(currentIndex)) {
          count += 1
          aggregated(currentIndex) = v
          found(currentIndex) = true
        }
      }
    }
  }
  trait CaseW[V] extends Writer[V]{
    def writeToObject[R](ctx: ObjVisitor[_, R],
                         out: upickle.jawn.Visitor[_, R],
                         v: V): Unit
    def write[R](out: upickle.jawn.Visitor[_, R], v: V): R = {
      if (v == null) out.jnull(-1)
      else{
        val ctx = out.objectContext(-1)
        writeToObject(ctx, out, v)
        ctx.finish(-1)
      }
    }
  }
  class SingletonR[T](t: T) extends CaseR[T](0){
    override def expectedMsg = "expected dictionary"
    override def objectContext(index: Int) = new ObjVisitor[Any, T] {
      def subVisitor = upickle.jawn.NullFacade

      def visitKey(s: CharSequence, index: Int): Unit = ???

      def add(v: Any, index: Int): Unit = ???

      def finish(index: Int) = t
    }
  }
  class SingletonW[T](f: T) extends CaseW[T] {
    def writeToObject[R](ctx: ObjVisitor[_, R], out: Visitor[_, R], v: T): Unit = () // do nothing
  }


  def taggedExpectedMsg: String
  def taggedArrayContext[T](taggedReader: TaggedReader[T], index: Int): ArrVisitor[Any, T] = throw new AbortJsonProcessingException(taggedExpectedMsg)
  def taggedObjectContext[T](taggedReader: TaggedReader[T], index: Int): ObjVisitor[Any, T] = throw new AbortJsonProcessingException(taggedExpectedMsg)
  def taggedWrite[T, R](w: CaseW[T], tag: String, out: Visitor[_, R], v: T): R

  private[this] def scanChildren[T, V](xs: Seq[T])(f: T => V) = {
    var x: V = null.asInstanceOf[V]
    val i = xs.iterator
    while(x == null && i.hasNext){
      val t = f(i.next())
      if(t != null) x = t
    }
    x
  }
  trait TaggedReader[T] extends Reader[T]{
    def findReader(s: String): Reader[T]

    override def expectedMsg = taggedExpectedMsg
    override def arrayContext(index: Int) = taggedArrayContext(this, index)
    override def objectContext(index: Int) = taggedObjectContext(this, index)
  }
  object TaggedReader{
    class Leaf[T](tag: String, r: Reader[T]) extends TaggedReader[T]{
      def findReader(s: String) = if (s == tag) r else null
    }
    class Node[T](rs: TaggedReader[_ <: T]*) extends TaggedReader[T]{
      def findReader(s: String) = scanChildren(rs)(_.findReader(s)).asInstanceOf[Reader[T]]
    }
  }

  trait TaggedWriter[T] extends Writer[T]{
    def findWriter(v: Any): (String, CaseW[T])
    def write[R](out: Visitor[_, R], v: T): R = {
      val (tag, w) = findWriter(v)
      taggedWrite(w, tag, out, v)

    }
  }
  object TaggedWriter{
    class Leaf[T](c: ClassTag[_], tag: String, r: CaseW[T]) extends TaggedWriter[T]{
      def findWriter(v: Any) = {
        if (c.runtimeClass.isInstance(v)) tag -> r
        else null
      }
    }
    class Node[T](rs: TaggedWriter[_ <: T]*) extends TaggedWriter[T]{
      def findWriter(v: Any) = scanChildren(rs)(_.findWriter(v)).asInstanceOf[(String, CaseW[T])]
    }
  }

  trait TaggedReadWriter[T] extends TaggedReader[T] with TaggedWriter[T]{
    override def narrow[K <: T] = this.asInstanceOf[ReadWriter[K]]
    override def arrayContext(index: Int) = taggedArrayContext(this, index)
    override def objectContext(index: Int) = taggedObjectContext(this, index)

  }
  object TaggedReadWriter{
    class Leaf[T](c: ClassTag[_], tag: String, r: CaseW[T] with Reader[T]) extends TaggedReadWriter[T]{
      def findReader(s: String) = if (s == tag) r else null
      def findWriter(v: Any) = {
        if (c.runtimeClass.isInstance(v)) (tag -> r)
        else null
      }
    }
    class Node[T](rs: TaggedReadWriter[_ <: T]*) extends TaggedReadWriter[T]{
      def findReader(s: String) = scanChildren(rs)(_.findReader(s)).asInstanceOf[Reader[T]]
      def findWriter(v: Any) = scanChildren(rs)(_.findWriter(v)).asInstanceOf[(String, CaseW[T])]
    }
  }

}