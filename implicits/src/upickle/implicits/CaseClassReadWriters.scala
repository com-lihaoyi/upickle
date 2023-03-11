package upickle.implicits

import scala.language.experimental.macros
import scala.language.higherKinds
import scala.reflect.ClassTag
import upickle.core.{Abort, AbortException, ArrVisitor, NoOpVisitor, ObjVisitor, Visitor}

/**
* Basic functionality to be able to read and write objects. Kept as a trait so
* other internal files can use it, while also mixing it into the `upickle`
* package to form the public API1
*/
trait CaseClassReadWriters extends upickle.core.Types{

  abstract class CaseClassReader[V] extends SimpleReader[V]{
    override def expectedMsg = "expected dictionary"
    override def visitString(s: CharSequence, index: Int) = visitObject(0, true, index).visitEnd(index)
  }
  trait CaseClassWriter[V] extends ObjectWriter[V]{
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
  class SingletonReader[T](t: T) extends CaseClassReader[T]{
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
  class SingletonWriter[T](f: T) extends CaseClassWriter[T] {
    def length(v: T) = 0
    def writeToObject[R](ctx: ObjVisitor[_, R], v: T): Unit = () // do nothing
  }


  trait TaggedReader[T] extends SimpleReader[T] {
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

  object TaggedReader {
    class Leaf[T](tag: String, r: Reader[T]) extends TaggedReader[T] {
      def findReader(s: String) = if (s == tag) r else null
    }

    class Node[T](rs: TaggedReader[_ <: T]*) extends TaggedReader[T] {
      def findReader(s: String) = scanChildren(rs)(_.findReader(s)).asInstanceOf[Reader[T]]
    }
  }

  trait TaggedWriter[T] extends Writer[T] {
    def findWriter(v: Any): (String, ObjectWriter[T])

    def write0[R](out: Visitor[_, R], v: T): R = {
      val (tag, w) = findWriter(v)
      taggedWrite(w, tag, out, v)

    }
  }

  object TaggedWriter {
    class Leaf[T](checker: Annotator.Checker, tag: String, r: ObjectWriter[T]) extends TaggedWriter[T] {
      def findWriter(v: Any) = {
        checker match {
          case Annotator.Checker.Cls(c) if c.isInstance(v) => tag -> r
          case Annotator.Checker.Val(v0) if v0 == v => tag -> r
          case _ => null
        }
      }
    }

    class Node[T](rs: TaggedWriter[_ <: T]*) extends TaggedWriter[T] {
      def findWriter(v: Any) = scanChildren(rs)(_.findWriter(v)).asInstanceOf[(String, ObjectWriter[T])]
    }
  }

  trait TaggedReadWriter[T] extends ReadWriter[T] with TaggedReader[T] with TaggedWriter[T] with SimpleReader[T] {
    override def visitArray(length: Int, index: Int) = taggedArrayContext(this, index)

    override def visitObject(length: Int, jsonableKeys: Boolean, index: Int) = taggedObjectContext(this, index)

  }

  object TaggedReadWriter {
    class Leaf[T](c: ClassTag[_], tag: String, r: ObjectWriter[T] with Reader[T]) extends TaggedReadWriter[T] {
      def findReader(s: String) = if (s == tag) r else null

      def findWriter(v: Any) = {
        if (c.runtimeClass.isInstance(v)) (tag -> r)
        else null
      }
    }

    class Node[T](rs: TaggedReadWriter[_ <: T]*) extends TaggedReadWriter[T] {
      def findReader(s: String) = scanChildren[TaggedReader[_], Reader[_]](rs)(_.findReader(s)).asInstanceOf[Reader[T]]

      def findWriter(v: Any) = scanChildren[TaggedWriter[_], (String, ObjectWriter[_])](rs)(_.findWriter(v)).asInstanceOf[(String, ObjectWriter[T])]
    }
  }

  def taggedExpectedMsg: String

  def taggedArrayContext[T](taggedReader: TaggedReader[T], index: Int): ArrVisitor[Any, T] = throw new Abort(taggedExpectedMsg)

  def taggedObjectContext[T](taggedReader: TaggedReader[T], index: Int): ObjVisitor[Any, T] = throw new Abort(taggedExpectedMsg)

  def taggedWrite[T, R](w: ObjectWriter[T], tag: String, out: Visitor[_, R], v: T): R

  private[this] def scanChildren[T, V](xs: Seq[T])(f: T => V) = {
    var x: V = null.asInstanceOf[V]
    val i = xs.iterator
    while (x == null && i.hasNext) {
      val t = f(i.next())
      if (t != null) x = t
    }
    x
  }

  trait ObjectWriter[T] extends Writer[T] {
    def length(v: T): Int

    def writeToObject[R](ctx: ObjVisitor[_, R], v: T): Unit
  }

  protected def mergeReadWriters[T](rws: ReadWriter[_ <: T]*): TaggedReadWriter[T] = {
    new TaggedReadWriter.Node(rws.asInstanceOf[Seq[TaggedReadWriter[T]]]: _*)
  }

  protected def mergeReaders[T](rws: Reader[_ <: T]*): TaggedReader[T] = {
    new TaggedReader.Node(rws.asInstanceOf[Seq[TaggedReader[T]]]: _*)
  }

  protected def mergeWriters[T](rws: Writer[_ <: T]*): TaggedWriter[T] = {
    new TaggedWriter.Node(rws.asInstanceOf[Seq[TaggedWriter[T]]]: _*)
  }

  protected def joinReadWriter[T](implicit r0: Reader[T], w0: Writer[T]): ReadWriter[T] = (r0, w0) match {
    case (r1: TaggedReader[T], w1: TaggedWriter[T]) =>
      new TaggedReadWriter[T] {
        override def isJsonDictKey = w0.isJsonDictKey

        def findReader(s: String) = r1.findReader(s)

        def findWriter(v: Any) = w1.findWriter(v)
      }

    case _ =>
      new Visitor.Delegate[Any, T](r0) with ReadWriter[T] {
        override def isJsonDictKey = w0.isJsonDictKey

        def write0[V](out: Visitor[_, V], v: T) = w0.write(out, v)
      }
  }

}


/**
 * Wrap a CaseClassReader or CaseClassWriter reader/writer to handle $type tags during reading and writing.
 *
 * Note that Scala 3 singleton `enum` values do not have proper `ClassTag[V]`s
 * like Scala 2 `case object`s do, so we instead use a `Checker.Val` to check
 * for `.equals` equality during writes to determine which tag to use.
 */
trait Annotator extends CaseClassReadWriters {
  def annotate[V](rw: Reader[V], n: String): TaggedReader[V]
  def annotate[V](rw: ObjectWriter[V], n: String, checker: Annotator.Checker): TaggedWriter[V]
  def annotate[V](rw: ObjectWriter[V], n: String)(implicit ct: ClassTag[V]): TaggedWriter[V] =
    annotate(rw, n, Annotator.Checker.Cls(ct.runtimeClass))
}
object Annotator{
  sealed trait Checker
  object Checker{
    case class Cls(c: Class[_]) extends Checker
    case class Val(v: Any) extends Checker
  }
}
