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

  object ReadWriter{
    abstract class Delegate[T](other: Visitor[Any, T])
      extends Visitor.Delegate[Any, T](other) with ReadWriter[T]

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
          override def isJsonDictKey = w0.isJsonDictKey
          def findReader(s: String) = r1.findReader(s)
          def findWriter(v: Any) = w1.findWriter(v)
        }

      case _ =>
        new Visitor.Delegate[Any, T](r0) with ReadWriter[T]{
          override def isJsonDictKey = w0.isJsonDictKey
          def write0[V](out: Visitor[_, V], v: T) = w0.write(out, v)
        }
    }
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
    def merge[T](readers0: Reader[_ <: T]*) = {
      new TaggedReader.Node(readers0.asInstanceOf[Seq[TaggedReader[T]]]:_*)
    }
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
    def merge[T](writers: Writer[_ <: T]*) = {
      new TaggedWriter.Node(writers.asInstanceOf[Seq[TaggedWriter[T]]]:_*)
    }
  }

  class TupleNWriter[V](val writers: Array[Writer[_]], val f: V => Array[Any]) extends Writer[V]{
    def write0[R](out: Visitor[_, R], v: V): R = {
      if (v == null) out.visitNull(-1)
      else{
        val ctx = out.visitArray(writers.length, -1)
        val vs = f(v)
        var i = 0
        while(i < writers.length){
          ctx.visitValue(
            writers(i).asInstanceOf[Writer[Any]].write(
              ctx.subVisitor.asInstanceOf[Visitor[Any, Nothing]],
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

  class TupleNReader[V](val readers: Array[Reader[_]], val f: Array[Any] => V) extends SimpleReader[V]{

    override def expectedMsg = "expected sequence"
    override def visitArray(length: Int, index: Int) = new ArrVisitor[Any, V] {
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
          throw new Abort(
            "expected " + readers.length + " items in sequence, found " + lengthSoFar
          )
        }
        start = facadesIndex

        f(b)

      }

      def subVisitor: Visitor[_, _] = {
        readers(facadesIndex % readers.length)
      }
    }
  }

  abstract class CaseR[V] extends SimpleReader[V]{
    override def expectedMsg = "expected dictionary"
    override def visitString(s: CharSequence, index: Int) = visitObject(0, true, index).visitEnd(index)
    trait BaseCaseObjectContext{
      def storeAggregatedValue(currentIndex: Int, v: Any): Unit
      def visitKey(index: Int) = _root_.upickle.core.StringVisitor
      var currentIndex = -1
      protected def storeValueIfNotFound(i: Int, v: Any): Unit
      protected def errorMissingKeys(rawArgsLength: Int, mappedArgs: Array[String]): Unit
      protected def checkErrorMissingKeys(rawArgsBitset: Long): Boolean
    }

    abstract class CaseObjectContext(fieldCount: Int) extends ObjVisitor[Any, V] with BaseCaseObjectContext{
      var found = 0L

      def visitValue(v: Any, index: Int): Unit = {
        if (currentIndex != -1 && ((found & (1L << currentIndex)) == 0)) {
          storeAggregatedValue(currentIndex, v)
          found |= (1L << currentIndex)
        }
      }

      protected def storeValueIfNotFound(i: Int, v: Any) = {
        if ((found & (1L << i)) == 0) {
          found |= (1L << i)
          storeAggregatedValue(i, v)
        }
      }
      protected def errorMissingKeys(rawArgsLength: Int, mappedArgs: Array[String]) = {
        val keys = for{
          i <- 0 until rawArgsLength
          if (found & (1L << i)) == 0
        } yield mappedArgs(i)
        throw new _root_.upickle.core.Abort(
          "missing keys in dictionary: " + keys.mkString(", ")
        )
      }
      protected def checkErrorMissingKeys(rawArgsBitset: Long) = {
        found != rawArgsBitset
      }
    }
    abstract class HugeCaseObjectContext(fieldCount: Int) extends ObjVisitor[Any, V] with BaseCaseObjectContext{
      var found = new Array[Long](fieldCount / 64 + 1)

      def visitValue(v: Any, index: Int): Unit = {
        if (currentIndex != -1 && ((found(currentIndex / 64) & (1L << currentIndex)) == 0)) {
          storeAggregatedValue(currentIndex, v)
          found(currentIndex / 64) |= (1L << currentIndex)
        }
      }

      protected def storeValueIfNotFound(i: Int, v: Any) = {
        if ((found(i / 64) & (1L << i)) == 0) {
          found(i / 64) |= (1L << i)
          storeAggregatedValue(i, v)
        }
      }
      protected def errorMissingKeys(rawArgsLength: Int, mappedArgs: Array[String]) = {
        val keys = for{
          i <- 0 until rawArgsLength
          if (found(i / 64) & (1L << i)) == 0
        } yield mappedArgs(i)
        throw new _root_.upickle.core.Abort(
          "missing keys in dictionary: " + keys.mkString(", ")
        )
      }
      protected def checkErrorMissingKeys(rawArgsLength: Long) = {
        var bits = 0
        for(v <- found) bits += java.lang.Long.bitCount(v)
        bits != rawArgsLength
      }
    }
  }
  trait CaseW[V] extends Writer[V]{
    def length(v: V): Int
    def writeToObject[R](ctx: ObjVisitor[_, R], v: V): Unit
    def write0[R](out: Visitor[_, R], v: V): R = {
      if (v == null) out.visitNull(-1)
      else{
        val ctx = out.visitObject(length(v), true, -1)
        writeToObject(ctx, v)
        ctx.visitEnd(-1)
      }
    }
    def writeSnippet[R, V](objectAttributeKeyWriteMap: CharSequence => CharSequence,
                           ctx: _root_.upickle.core.ObjVisitor[_, R],
                           mappedArgsI: String,
                           w: Any,
                           value: Any) = {
      val keyVisitor = ctx.visitKey(-1)
      ctx.visitKeyValue(
        keyVisitor.visitString(objectAttributeKeyWriteMap(mappedArgsI), -1)
      )
      ctx.narrow.visitValue(w.asInstanceOf[Writer[Any]].write(ctx.subVisitor, value), -1)
    }
  }
  class SingletonR[T](t: T) extends CaseR[T]{
    override def expectedMsg = "expected string or dictionary"

    override def visitString(s: CharSequence, index: Int) = t

    override def visitObject(length: Int, jsonableKeys: Boolean, index: Int) = new ObjVisitor[Any, T] {
      def subVisitor = NoOpVisitor

      def visitKey(index: Int) = NoOpVisitor
      def visitKeyValue(s: Any) = ()

      def visitValue(v: Any, index: Int): Unit = ()

      def visitEnd(index: Int) = t
    }
  }
  class SingletonW[T](f: T) extends CaseW[T] {
    def length(v: T) = 0
    def writeToObject[R](ctx: ObjVisitor[_, R], v: T): Unit = () // do nothing
  }


  def taggedExpectedMsg: String
  def taggedArrayContext[T](taggedReader: TaggedReader[T], index: Int): ArrVisitor[Any, T] = throw new Abort(taggedExpectedMsg)
  def taggedObjectContext[T](taggedReader: TaggedReader[T], index: Int): ObjVisitor[Any, T] = throw new Abort(taggedExpectedMsg)
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
  trait TaggedReader[T] extends SimpleReader[T]{
    def findReader(s: String): Reader[T]

    override def expectedMsg = taggedExpectedMsg
    override def visitArray(length: Int, index: Int) = taggedArrayContext(this, index)
    override def visitObject(length: Int, jsonableKeys: Boolean, index: Int) = taggedObjectContext(this, index)
    override def visitString(s: CharSequence, index: Int) = findReader(s.toString).visitString(s, index)
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

  trait TaggedReadWriter[T] extends ReadWriter[T] with TaggedReader[T] with TaggedWriter[T] with SimpleReader[T]{
    override def visitArray(length: Int, index: Int) = taggedArrayContext(this, index)
    override def visitObject(length: Int, jsonableKeys: Boolean, index: Int) = taggedObjectContext(this, index)

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

trait Annotator { this: Types =>
  def annotate[V](rw: CaseR[V], n: String): TaggedReader[V]
  def annotate[V](rw: CaseW[V], n: String)(implicit c: ClassTag[V]): TaggedWriter[V]
}
