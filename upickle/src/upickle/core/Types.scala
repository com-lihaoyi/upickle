package upickle
package core

import ujson._

import scala.language.experimental.macros
import scala.language.higherKinds
import scala.reflect.ClassTag

/**
* Basic functionality to be able to read and write objects. Kept as a trait so
* other internal files can use it, while also mixing it into the `upickle`
* package to form the public API1
*/
trait Types{ types =>
  trait ReadWriter[T] extends Reader[T] with Writer[T]{
    override def narrow[K <: T] = this.asInstanceOf[ReadWriter[K]]
    def bimap[V](f: V => T, g: T => V): ReadWriter[V] = {
      new ReadWriter[V] with BaseReader.MapReader[Any, T, V]{
        override def delegatedReader = ReadWriter.this
        def write0[Z](out: Visitor[_, Z], v: V) = {
          ReadWriter.this.write(out, f(v.asInstanceOf[V]))
        }

        override def mapNonNullsFunction(t: T) = g(t)
      }
    }
  }

  object ReadWriter{

    def merge[T](rws: ReadWriter[_ <: T]*): TaggedReadWriter[T] = {
      new TaggedReadWriter.Node(rws.asInstanceOf[Seq[TaggedReadWriter[T]]]:_*)
    }

    implicit def join[T](implicit r0: Reader[T], w0: Writer[T]): ReadWriter[T] = (r0, w0) match{
      // Make sure we preserve the tagged-ness of the Readers/Writers being
      // pulled in; we need to do this because the macros that generate tagged
      // Readers/Writers do not know until post-typechecking whether or not the
      // Reader/Writer needs to be tagged, and thus cannot communicate that
      // fact in the returned type of the macro call. Thus we are forced to
      // wait until runtime before inspecting it and seeing if the tags exist

      case (r1: TaggedReader[T], w1: TaggedWriter[T]) =>
        new TaggedReadWriter[T] {
          def findReader(s: String) = r1.findReader(s)
          def findWriter(v: Any) = w1.findWriter(v)
        }

      case _ =>
        new BaseReader.Delegate[Any, T] with ReadWriter[T]{
          def delegatedReader = r0
          def write0[V](out: Visitor[_, V], v: T) = w0.write(out, v)
        }
    }
  }

  object Reader{

    def merge[T](readers0: Reader[_ <: T]*) = {
      new TaggedReader.Node(readers0.asInstanceOf[Seq[TaggedReader[T]]]:_*)
    }
  }
  type Reader[T] = BaseReader[Any, T]
  trait BaseReader[-T, V] extends ujson.CustomVisitor[T, V] {
    def expectedMsg = ""
    def narrow[K <: V] = this.asInstanceOf[BaseReader[T, K]]

    def map[Z](f: V => Z) = new BaseReader.MapReader[T, V, Z]{
      override def delegatedReader = BaseReader.this
      def mapNonNullsFunction(v: V): Z = f(v)
    }
    def mapNulls[Z](f: V => Z) = new BaseReader.MapReader[T, V, Z]{
      override def delegatedReader = BaseReader.this
      override def mapFunction(v: V): Z = f(v)
      def mapNonNullsFunction(v: V): Z = f(v)
    }
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

      override def visitNumRaw(d: Double, index: Int) = {
        delegatedReader.visitNumRaw(d, index)
      }
      override def visitObject(index: Int) = delegatedReader.visitObject(index)
      override def visitArray(index: Int) = delegatedReader.visitArray(index)
    }

    trait MapReader[-T, V, Z] extends BaseReader[T, Z] {
      def delegatedReader: BaseReader[T, V]
      def mapNonNullsFunction(t: V) : Z
      def mapFunction(v: V): Z =
        if(v == null) null.asInstanceOf[Z]
        else mapNonNullsFunction(v)

      override def visitFalse(index: Int) = mapFunction(delegatedReader.visitFalse(index))
      override def visitNull(index: Int) = mapFunction(delegatedReader.visitNull(index))
      override def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
        mapFunction(delegatedReader.visitNum(s, decIndex, expIndex, index))
      }
      override def visitNumRaw(d: Double, index: Int) = {
        mapFunction(delegatedReader.visitNumRaw(d, index))
      }
      override def visitString(s: CharSequence, index: Int) = {
        mapFunction(delegatedReader.visitString(s, index))
      }
      override def visitTrue(index: Int) = mapFunction(delegatedReader.visitTrue(index))

      override def visitObject(index: Int): ujson.ObjVisitor[T, Z] = {
        new MapObjContext[T, V, Z](delegatedReader.visitObject(index), mapNonNullsFunction)
      }
      override def visitArray(index: Int): ujson.ArrVisitor[T, Z] = {
        new MapArrContext[T, V, Z](delegatedReader.visitArray(index), mapNonNullsFunction)
      }
    }

    class MapArrContext[T, V, Z](src: ujson.ArrVisitor[T, V],
                                 f: V => Z) extends ujson.ArrVisitor[T, Z]{
      def subVisitor = src.subVisitor

      def visitValue(v: T, index: Int): Unit = src.visitValue(v, index)

      def visitEnd(index: Int) = f(src.visitEnd(index))
    }

    class MapObjContext[T, V, Z](src: ujson.ObjVisitor[T, V],
                                 f: V => Z) extends ujson.ObjVisitor[T, Z]{
      def subVisitor = src.subVisitor

      def visitKey(s: CharSequence, index: Int) = src.visitKey(s, index)

      def visitValue(v: T, index: Int): Unit = src.visitValue(v, index)

      def visitEnd(index: Int) = f(src.visitEnd(index))
    }
  }
  trait Writer[T] extends Transformer[T]{
    def narrow[K <: T] = this.asInstanceOf[Writer[K]]
    def transform[V](v: T, out: ujson.Visitor[_, V]) = write(out, v)
    def write0[V](out: ujson.Visitor[_, V], v: T): V
    def write[V](out: ujson.Visitor[_, V], v: T): V = {
      if (v == null) out.visitNull(-1)
      else write0(out, v)
    }
    def comapNulls[U](f: U => T) = new Writer.MapWriterNulls[U, T](this, f)
    def comap[U](f: U => T) = new Writer.MapWriter[U, T](this, f)
  }
  object Writer {

    class MapWriterNulls[U, T](src: Writer[T], f: U => T) extends Writer[U] {
      override def write[R](out: ujson.Visitor[_, R], v: U): R = src.write(out, f(v))
      def write0[R](out: ujson.Visitor[_, R], v: U): R = src.write(out, f(v))
    }
    class MapWriter[U, T](src: Writer[T], f: U => T) extends Writer[U] {
      def write0[R](out: ujson.Visitor[_, R], v: U): R = src.write(out, f(v))
    }
    def merge[T](writers: Writer[_ <: T]*) = {
      new TaggedWriter.Node(writers.asInstanceOf[Seq[TaggedWriter[T]]]:_*)
    }
  }

  class TupleNWriter[V](val writers: Array[Writer[_]], val f: V => Array[Any]) extends Writer[V]{
    def write0[R](out: ujson.Visitor[_, R], v: V): R = {
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
    override def visitArray(index: Int) = new ujson.ArrVisitor[Any, V] {
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
    trait CaseObjectContext extends ujson.ObjVisitor[Any, V]{
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
                         v: V): Unit
    def write0[R](out: ujson.Visitor[_, R], v: V): R = {
      if (v == null) out.visitNull(-1)
      else{
        val ctx = out.visitObject(-1)
        writeToObject(ctx, v)
        ctx.visitEnd(-1)
      }
    }
  }
  class SingletonR[T](t: T) extends CaseR[T](0){
    override def expectedMsg = "expected dictionary"
    override def visitObject(index: Int) = new ObjVisitor[Any, T] {
      def subVisitor = ujson.NoOpVisitor

      def visitKey(s: CharSequence, index: Int): Unit = ???

      def visitValue(v: Any, index: Int): Unit = ???

      def visitEnd(index: Int) = t
    }
  }
  class SingletonW[T](f: T) extends CaseW[T] {
    def writeToObject[R](ctx: ObjVisitor[_, R], v: T): Unit = () // do nothing
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
    def write0[R](out: Visitor[_, R], v: T): R = {
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

  trait TaggedReadWriter[T] extends ReadWriter[T] with TaggedReader[T] with TaggedWriter[T]{
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