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
    def visitNull(index: Int): V = null.asInstanceOf[V]
    def visitTrue(index: Int): V =  throw new AbortJsonProcessingException(expectedMsg + " got boolean")
    def visitFalse(index: Int): V = throw new AbortJsonProcessingException(expectedMsg + " got boolean")

    def visitString(s: CharSequence, index: Int): V = {
      throw new AbortJsonProcessingException(expectedMsg + " got string")
    }
    def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): V = {
      throw new AbortJsonProcessingException(expectedMsg + " got number")
    }

    def visitObject(index: Int): upickle.jawn.ObjVisitor[T, V] = {
      throw new AbortJsonProcessingException(expectedMsg + " got dictionary")
    }
    def visitArray(index: Int): upickle.jawn.ArrVisitor[T, V] = {
      throw new AbortJsonProcessingException(expectedMsg + " got sequence")
    }
    def map[Z](f: V => Z) = new BaseReader.MapReader[T, V, Z](this, f)
    def mapNulls[Z](f: V => Z) = new BaseReader.MapReaderNullable[T, V, Z](this, f)
  }

  object BaseReader {
    trait Delegate[T, V] extends BaseReader[T, V]{
      def delegatedReader: BaseReader[T, V]

      override def visitNull(index: Int) = delegatedReader.visitNull(index)
      override def visitTrue(index: Int) = delegatedReader.visitTrue(index)
      override def visitFalse(index: Int) = delegatedReader.visitFalse(index)

      override def visitString(s: CharSequence, index: Int) = delegatedReader.visitString(s, index)
      override def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
        delegatedReader.visitNum(s, decIndex, expIndex, index)
      }

      override def visitObject(index: Int) = delegatedReader.visitObject(index)
      override def visitArray(index: Int) = delegatedReader.visitArray(index)
    }
    class MapReaderNullable[-T, V, Z](src: BaseReader[T, V], f: V => Z) extends MapReader[T, V, Z](src, f ){
      override def f1(v: V): Z = f(v)
    }
    class MapReader[-T, V, Z](src: BaseReader[T, V], f: V => Z) extends BaseReader[T, Z] {
      def f1(v: V): Z = if(v == null) null.asInstanceOf[Z] else f(v)

      override def visitFalse(index: Int) = f1(src.visitFalse(index))
      override def visitNull(index: Int) = f1(src.visitNull(index))
      override def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
        f1(src.visitNum(s, decIndex, expIndex, index))
      }
      override def visitString(s: CharSequence, index: Int) = {
        f1(src.visitString(s, index))
      }
      override def visitTrue(index: Int) = f(src.visitTrue(index))

      override def visitObject(index: Int): upickle.jawn.ObjVisitor[T, Z] = {
        new MapObjContext[T, V, Z](src.visitObject(index), f)
      }
      override def visitArray(index: Int): upickle.jawn.ArrVisitor[T, Z] = {
        new MapArrContext[T, V, Z](src.visitArray(index), f)
      }

      // We do not override the singleContext with a MapFContext, because
      // unlike array/object-Contexts, the value being returned by the `visitEnd`
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

      def visitValue(v: T, index: Int): Unit = src.visitValue(v, index)

      def visitEnd(index: Int) = f(src.visitEnd(index))
    }

    class MapObjContext[T, V, Z](src: upickle.jawn.ObjVisitor[T, V],
                               f: V => Z) extends upickle.jawn.ObjVisitor[T, Z]{
      def subVisitor = src.subVisitor

      def visitKey(s: CharSequence, index: Int) = src.visitKey(s, index)

      def visitValue(v: T, index: Int): Unit = src.visitValue(v, index)

      def visitEnd(index: Int) = f(src.visitEnd(index))
    }
  }
  trait Writer[T] extends Transformer[T]{
    def narrow[K <: T] = this.asInstanceOf[Writer[K]]
    def transform[V](v: T, out: upickle.jawn.Visitor[_, V]) = write(out, v)
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
        if (v == null) out.visitNull(-1)
        else src.write(out, f(v))
      }
    }
    def merge[T](writers: Writer[_ <: T]*) = {
      new TaggedWriter.Node(writers.asInstanceOf[Seq[TaggedWriter[T]]]:_*)
    }
  }

  class TupleNWriter[V](val writers: Array[Writer[_]], val f: V => Array[Any]) extends Writer[V]{
    def write[R](out: upickle.jawn.Visitor[_, R], v: V): R = {
      if (v == null) out.visitNull(-1)
      else{
        val ctx = out.visitArray()
        val vs = f(v)
        var i = 0
        while(i < writers.length){
          ctx.visitValue(
            writers(i).asInstanceOf[Writer[Any]].write(
              out.asInstanceOf[Visitor[Any, Nothing]],
              vs(i)
            ),
            -1
          )
          i += 1
        }
        ctx.visitEnd(-1)
      }
    }
  }

  class TupleNReader[V](val readers: Array[Reader[_]], val f: Array[Any] => V) extends Reader[V]{

    override def expectedMsg = "expected sequence"
    override def visitArray(index: Int) = new upickle.jawn.ArrVisitor[Any, V] {
      val b = new Array[Any](readers.length)
      var facadesIndex = 0

      var start = facadesIndex
      def visitValue(v: Any, index: Int): Unit = {
        b(facadesIndex % readers.length) = v
        facadesIndex = facadesIndex + 1
      }

      def visitEnd(index: Int) = {
        val lengthSoFar = facadesIndex - start
        if (lengthSoFar != readers.length) {
          throw new AbortJsonProcessingException(
            "expected " + readers.length + " items in sequence, found " + lengthSoFar
          )
        }
        start = facadesIndex

        f(b)

      }

      def subVisitor = {
        readers(facadesIndex % readers.length)
      }
    }
  }

  abstract class CaseR[V](val argCount: Int) extends Reader[V]{
    override def expectedMsg = "expected dictionary"
    trait CaseObjectContext extends upickle.jawn.ObjVisitor[Any, V]{
      val aggregated = new Array[Any](argCount)
      val found = new Array[Boolean](argCount)
      var currentIndex = -1
      var count = 0
      def visitValue(v: Any, index: Int): Unit = {
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
      if (v == null) out.visitNull(-1)
      else{
        val ctx = out.visitObject(-1)
        writeToObject(ctx, ctx.subVisitor, v)
        ctx.visitEnd(-1)
      }
    }
  }
  class SingletonR[T](t: T) extends CaseR[T](0){
    override def expectedMsg = "expected dictionary"
    override def visitObject(index: Int) = new ObjVisitor[Any, T] {
      def subVisitor = upickle.jawn.NoOpVisitor

      def visitKey(s: CharSequence, index: Int): Unit = ???

      def visitValue(v: Any, index: Int): Unit = ???

      def visitEnd(index: Int) = t
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
    override def visitArray(index: Int) = taggedArrayContext(this, index)
    override def visitObject(index: Int) = taggedObjectContext(this, index)
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
    override def visitArray(index: Int) = taggedArrayContext(this, index)
    override def visitObject(index: Int) = taggedObjectContext(this, index)

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