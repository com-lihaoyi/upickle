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

    def merge[T](tagKey: String, rws: ReadWriter[_ <: T]*): TaggedReadWriter[T] = {
      new TaggedReadWriter.Node(tagKey, rws.asInstanceOf[Seq[TaggedReadWriter[T]]]:_*)
    }

    def merge[T](rws: ReadWriter[_ <: T]*): TaggedReadWriter[T] = merge(Annotator.defaultTagKey, rws:_*)

    implicit def join[T](implicit r0: Reader[T], w0: Writer[T]): ReadWriter[T] = (r0, w0) match{
      // Make sure we preserve the tagged-ness of the Readers/Writers being
      // pulled in; we need to do this because the macros that generate tagged
      // Readers/Writers do not know until post-typechecking whether or not the
      // Reader/Writer needs to be tagged, and thus cannot communicate that
      // fact in the returned type of the macro call. Thus we are forced to
      // wait until runtime before inspecting it and seeing if the tags exist

      case (r1: TaggedReader[T], w1: TaggedWriter[T]) =>
        new TaggedReadWriter[T] {
          private[upickle] override def tagKey = r1.tagKey
          override def isJsonDictKey = w0.isJsonDictKey
          def findReader(s: String) = r1.findReader(s)
          @deprecated("Not used, left for binary compatibility")
          def findWriter(v: Any) = w1.findWriter(v)
          override def findWriterWithKey(v: Any) = w1.findWriterWithKey(v)
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
    def merge[T](tagKey: String, readers0: Reader[_ <: T]*): TaggedReader.Node[T] = {
      new TaggedReader.Node(tagKey, readers0.asInstanceOf[Seq[TaggedReader[T]]]:_*)
    }

    def merge[T](readers0: Reader[_ <: T]*): TaggedReader.Node[T] = merge(Annotator.defaultTagKey, readers0:_*)
  }

  /**
    * Represents the ability to write a value of type [[T]].
    *
    * Generally nothing more than a way of applying the [[T]] to
    * a [[Visitor]], along with some utility methods
    */
  trait Writer[T] extends Transformer[T]{
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

  trait TaggedReader[T] extends SimpleReader[T]{
    private[upickle] def tagKey: String = Annotator.defaultTagKey

    def findReader(s: String): Reader[T]

    override def expectedMsg = taggedExpectedMsg
    override def visitArray(length: Int, index: Int) = taggedArrayContext(this, index)
    override def visitObject(length: Int, jsonableKeys: Boolean, index: Int) = taggedObjectContext(this, index)
    override def visitString(s: CharSequence, index: Int) = {
      findReader(s.toString) match {
        case null => throw new AbortException("invalid tag for tagged object: " + s, index, -1, -1, null)
        case reader => reader.visitString(s, index)
      }
    }
  }
  object TaggedReader{
    class Leaf[T](private[upickle] override val tagKey: String,
                  tagValue: String,
                  tagShortValue: String,
                  r: Reader[T]) extends TaggedReader[T]{
      @deprecated("Not used, left for binary compatibility")
      def this(tag: String, r: Reader[T]) = this(Annotator.defaultTagKey, tag, tag, r)
      def this(tagKey: String, tagValue: String, r: Reader[T]) = this(tagKey, tagValue, tagValue, r)

      def findReader(s: String) = if (s == tagValue || s == tagShortValue) r else null
    }
    class Node[T](private[upickle] override val tagKey: String, rs: TaggedReader[_ <: T]*) extends TaggedReader[T]{
      @deprecated("Not used, left for binary compatibility")
      def this(rs: TaggedReader[_ <: T]*) = this(Annotator.defaultTagKey, rs:_*)

      def findReader(s: String) = scanChildren(rs)(_.findReader(s)).asInstanceOf[Reader[T]]
    }
  }

  trait TaggedWriter[T] extends Writer[T]{
    @deprecated("Not used, left for binary compatibility")
    def findWriter(v: Any): (String, ObjectWriter[T])

    // Calling deprecated method to maintain binary compatibility
    @annotation.nowarn("msg=deprecated")
    def findWriterWithKey(v: Any): (String, String, ObjectWriter[T]) = {
      val (tag, w) = findWriter(v)
      (Annotator.defaultTagKey, tag, w)
    }

    def write0[R](out: Visitor[_, R], v: T): R = {
      val (tagKey, tagValue, w) = findWriterWithKey(v)
      taggedWrite(w, tagKey, tagValue, out, v)

    }
  }
  object TaggedWriter{
    class Leaf[T](checker: Annotator.Checker,
                  tagKey: String,
                  tagValue: String,
                  r: ObjectWriter[T]) extends TaggedWriter[T]{
      @deprecated("Not used, left for binary compatibility")
      def this(checker: Annotator.Checker, tag: String, r: ObjectWriter[T]) =
        this(checker, Annotator.defaultTagKey, tag, r)

      @deprecated("Not used, left for binary compatibility")
      def findWriter(v: Any) = {
        checker match{
          case Annotator.Checker.Cls(c) if c.isInstance(v) => tagValue -> r
          case Annotator.Checker.Val(v0) if v0 == v => tagValue -> r
          case _ => null
        }
      }

      override def findWriterWithKey(v: Any) = {
        checker match{
          case Annotator.Checker.Cls(c) if c.isInstance(v) => (tagKey, tagValue, r)
          case Annotator.Checker.Val(v0) if v0 == v => (tagKey, tagValue, r)
          case _ => null
        }
      }
    }
    class Node[T](rs: TaggedWriter[_ <: T]*) extends TaggedWriter[T]{
      @deprecated("Not used, left for binary compatibility")
      def findWriter(v: Any) = scanChildren(rs)(_.findWriter(v)).asInstanceOf[(String, ObjectWriter[T])]
      override def findWriterWithKey(v: Any) =
        scanChildren(rs)(_.findWriterWithKey(v)).asInstanceOf[(String, String, ObjectWriter[T])]
    }
  }

  trait TaggedReadWriter[T] extends ReadWriter[T] with TaggedReader[T] with TaggedWriter[T] with SimpleReader[T]{
    override def visitArray(length: Int, index: Int) = taggedArrayContext(this, index)
    override def visitObject(length: Int, jsonableKeys: Boolean, index: Int) = taggedObjectContext(this, index)
  }

  object TaggedReadWriter{
    class Leaf[T](c: ClassTag[_],
                  private[upickle] override val tagKey: String,
                  tagValue: String, r: ObjectWriter[T] with Reader[T]) extends TaggedReadWriter[T]{
      @deprecated("Not used, left for binary compatibility")
      def this(c: ClassTag[_], tag: String, r: ObjectWriter[T] with Reader[T]) = this(c, Annotator.defaultTagKey, tag, r)

      def findReader(s: String) = if (s == tagValue) r else null
      @deprecated("Not used, left for binary compatibility")
      def findWriter(v: Any) = {
        if (c.runtimeClass.isInstance(v)) tagValue -> r
        else null
      }
      override def findWriterWithKey(v: Any) = {
        if (c.runtimeClass.isInstance(v)) (tagKey, tagValue, r)
        else null
      }
    }
    class Node[T](private[upickle] override val tagKey: String, rs: TaggedReadWriter[_ <: T]*) extends TaggedReadWriter[T]{
      @deprecated("Not used, left for binary compatibility")
      def this(rs: TaggedReadWriter[_ <: T]*) = this(Annotator.defaultTagKey, rs:_*)

      def findReader(s: String) = scanChildren(rs)(_.findReader(s)).asInstanceOf[Reader[T]]
      @deprecated("Not used, left for binary compatibility")
      def findWriter(v: Any) = scanChildren(rs)(_.findWriter(v)).asInstanceOf[(String, ObjectWriter[T])]
      override def findWriterWithKey(v: Any) =
        scanChildren(rs)(_.findWriterWithKey(v)).asInstanceOf[(String, String, ObjectWriter[T])]
    }
  }

  def taggedExpectedMsg: String

  def taggedArrayContext[T](taggedReader: TaggedReader[T], index: Int): ArrVisitor[Any, T] = throw new Abort(taggedExpectedMsg)

  def taggedObjectContext[T](taggedReader: TaggedReader[T], index: Int): ObjVisitor[Any, T] = throw new Abort(taggedExpectedMsg)

  @deprecated("Not used, left for binary compatibility")
  def taggedWrite[T, R](w: ObjectWriter[T], tag: String, out: Visitor[_,  R], v: T): R

  // Calling deprecated method to maintain binary compatibility
  @annotation.nowarn("msg=deprecated")
  def taggedWrite[T, R](w: ObjectWriter[T], tagKey: String, tagValue: String, out: Visitor[_, R], v: T): R =
    taggedWrite(w, tagValue, out, v)

  private[this] def scanChildren[T, V](xs: Seq[T])(f: T => V) = {
    var x: V = null.asInstanceOf[V]
    val i = xs.iterator
    while (x == null && i.hasNext) {
      val t = f(i.next())
      if (t != null) x = t
    }
    x
  }

  trait ObjectWriter[T] extends Writer[T]{
    def length(v: T): Int
    def writeToObject[R](ctx: ObjVisitor[_, R], v: T): Unit
  }
}

/**
 * Implicit to indicate that we are currently deriving an implicit [[T]]. Used
 * to avoid the implicit being derived from picking up its own definition,
 * resulting in infinite looping/recursion
 */
class CurrentlyDeriving[T]

/**
 * Wrap a CaseClassReader or CaseClassWriter reader/writer to handle $type tags during reading and writing.
 *
 * Note that Scala 3 singleton `enum` values do not have proper `ClassTag[V]`s
 * like Scala 2 `case object`s do, so we instead use a `Checker.Val` to check
 * for `.equals` equality during writes to determine which tag to use.
 */
@scala.annotation.nowarn("msg=deprecated")
trait Annotator { this: Types =>

  @deprecated("Not used, left for binary compatibility")
  def annotate[V](rw: Reader[V], n: String): TaggedReader[V]

  @deprecated("Not used, left for binary compatibility")
  def annotate[V](rw: Reader[V], key: String, value: String): TaggedReader[V] = annotate(rw, value, value)

  def annotate[V](rw: Reader[V], key: String, value: String, shortValue: String): TaggedReader[V] = annotate(rw, value)

  @deprecated("Not used, left for binary compatibility")
  def annotate[V](rw: ObjectWriter[V], n: String, checker: Annotator.Checker): TaggedWriter[V]

  @deprecated("Not used, left for binary compatibility")
  def annotate[V](rw: ObjectWriter[V], key: String, value: String, checker: Annotator.Checker): TaggedWriter[V] =
    annotate(rw, key, value, value, checker)

  def annotate[V](rw: ObjectWriter[V], key: String, value: String, shortValue: String, checker: Annotator.Checker): TaggedWriter[V] =
    annotate(rw, value, checker)

  @deprecated("Not used, left for binary compatibility")
  def annotate[V](rw: ObjectWriter[V], key: String, value: String)(implicit ct: ClassTag[V]): TaggedWriter[V] =
    annotate(rw, key, value, value)(ct)

  def annotate[V](rw: ObjectWriter[V], key: String, value: String, shortValue: String)(implicit ct: ClassTag[V]): TaggedWriter[V] =
    annotate(rw, key, value, shortValue, Annotator.Checker.Cls(ct.runtimeClass))

  @deprecated("Not used, left for binary compatibility")
  final def annotate[V](rw: ObjectWriter[V], n: String)(implicit ct: ClassTag[V]): TaggedWriter[V] =
    annotate(rw, Annotator.defaultTagKey, n, n, Annotator.Checker.Cls(ct.runtimeClass))
}
object Annotator{
  def defaultTagKey = "$type"

  sealed trait Checker
  object Checker{
    case class Cls(c: Class[_]) extends Checker
    case class Val(v: Any) extends Checker
  }
}
