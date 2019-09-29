package upickle.core

import upickle.core.JsonPointerVisitor._

import scala.annotation.tailrec
import scala.util.control.{NoStackTrace, NonFatal}

/**
  * Adds a JSON Pointer to exceptions thrown by the delegate Visitor.
  *
  * JSON Pointer is standardized by RFC 6901 and commonly used by JSON Schema.
  *
  * Useful for debugging failures.
  * Adds ~10% overhead depending on the parser.
  *
  * @see https://tools.ietf.org/html/rfc6901
  */
object JsonPointerVisitor {

  def apply[T, J](delegate: Visitor[T, J]): Visitor[T, J] = new JsonPointerVisitor(delegate, RootHasPath)

  /**
    * JSON Pointer indicating where the problem occurred.
    * Added as a suppressed exception.
    *
    * @param jsonPointer e.g. "/hits/hits/3/_source/foo/bar"
    * @see https://tools.ietf.org/html/rfc6901
    */
  class JsonPointerException(val jsonPointer: String, cause: Throwable) extends Exception(jsonPointer, cause) with NoStackTrace {

    override def toString: String = jsonPointer
  }

  /**
    * Internally, the paths form a linked list back to the root by the visitors themselves.
    * Compared to something like a List[String] or List[Object], this does not require
    * extra String allocation or boxing unless we actually ask for the path.
    */
  private trait HasPath {

    /**
      * Forms a chain toward the root.
      */
    def parent: Option[HasPath]

    /**
      * @return name of a single level, if any, e.g. "foo"
      */
    def pathComponent: Option[String]

    /**
      * @return root-to-leaf ordered chain as a List[HasPath]
      */
    private def components: List[HasPath] = {
      // Used rarely. Stack safety > memory efficiency here.
      @tailrec def listPath(o: Option[HasPath], list: List[HasPath]): List[HasPath] = {
        o match {
          case Some(p) => listPath(p.parent, p :: list)
          case None => list
        }
      }
      listPath(parent, List(this))
    }

    /**
      * @return the full path, slash-delimited.
      */
    def path: String = components.iterator.map(_.pathComponent).flatten.mkString("/")

    override def toString: String = path

    def wrap[T](f: => T): T = {
      try {
        f
      } catch {
        case NonFatal(cause) =>
          throw new JsonPointerException(path, cause)
      }
    }
  }

  private object RootHasPath extends HasPath {

    override def pathComponent: Option[String] = Some("")

    override def parent: Option[HasPath] = None
  }

}

private class JsonPointerVisitor[T, J](
  protected val delegate: Visitor[T, J],
  parentPath: HasPath
) extends Visitor.Delegate[T, J](delegate) {

  override def visitObject(length: Int, index: Int): ObjVisitor[T, J] = {
    val objVisitor = parentPath.wrap(super.visitObject(length, index))
    new ObjVisitor[T, J] with HasPath {
      private var key: String = _

      override def visitKey(index: Int): Visitor[_, _] = new JsonPointerVisitor[Nothing, Any](objVisitor.visitKey(index), this) {
        override def visitString(s: CharSequence, index: Int): Any = {
          key = s.toString
          wrap(this.delegate.visitString(key, index))
        }
      }

      override def visitKeyValue(v: Any): Unit = {
        if (key == null) key = "?"
        wrap(objVisitor.visitKeyValue(v))
      }

      override def subVisitor: Visitor[Nothing, Any] = {
        new JsonPointerVisitor(objVisitor.subVisitor.asInstanceOf[Visitor[T, J]], this)
      }

      override def visitValue(v: T, index: Int): Unit = {
        key = null // reset before visitEnd.
        wrap(objVisitor.visitValue(v, index))
      }

      override def visitEnd(index: Int): J = {
        wrap(objVisitor.visitEnd(index))
      }

      override def pathComponent: Option[String] = Option(key).map(_.replaceAllLiterally("~", "~0").replaceAllLiterally("/", "~1"))

      override def parent: Option[HasPath] = Some(JsonPointerVisitor.this.parentPath)
    }
  }

  override def visitArray(length: Int, index: Int): ArrVisitor[T, J] = {
    val arrVisitor = parentPath.wrap(super.visitArray(length, index))
    new ArrVisitor[T, J] with HasPath {
      private var i = -1

      override def subVisitor: Visitor[Nothing, Any] = {
        i += 1
        new JsonPointerVisitor(arrVisitor.subVisitor.asInstanceOf[Visitor[T, J]], this)
      }

      override def visitValue(v: T, index: Int): Unit = {
        wrap(arrVisitor.visitValue(v, index))
      }

      override def visitEnd(index: Int): J = {
        i = -1
        wrap(arrVisitor.visitEnd(index))
      }

      override def pathComponent: Option[String] = if (i >= 0) Some(i.toString) else None

      override def parent: Option[HasPath] = Some(JsonPointerVisitor.this.parentPath)
    }
  }

  override def visitNull(index: Int): J = parentPath.wrap(super.visitNull(index))

  override def visitTrue(index: Int): J = parentPath.wrap(super.visitTrue(index))

  override def visitFalse(index: Int): J = parentPath.wrap(super.visitFalse(index))

  override def visitString(s: CharSequence, index: Int): J = parentPath.wrap(super.visitString(s, index))

  override def visitFloat64StringParts(
    s: CharSequence,
    decIndex: Int,
    expIndex: Int,
    index: Int
  ): J = parentPath.wrap(super.visitFloat64StringParts(s, decIndex, expIndex, index))

  override def visitFloat64(d: Double, index: Int): J = parentPath.wrap(super.visitFloat64(d, index))

  override def visitFloat32(d: Float, index: Int): J = parentPath.wrap(super.visitFloat32(d, index))

  override def visitInt32(i: Int, index: Int): J = parentPath.wrap(super.visitInt32(i, index))

  override def visitInt64(i: Long, index: Int): J = parentPath.wrap(super.visitInt64(i, index))

  override def visitUInt64(i: Long, index: Int): J = parentPath.wrap(super.visitUInt64(i, index))

  override def visitFloat64String(s: String, index: Int): J = parentPath.wrap(super.visitFloat64String(s, index))

  override def visitChar(s: Char, index: Int): J = parentPath.wrap(super.visitChar(s, index))

  override def visitBinary(bytes: Array[Byte], offset: Int, len: Int, index: Int): J = parentPath.wrap(super.visitBinary(bytes, offset, len, index))

  override def visitExt(tag: Byte, bytes: Array[Byte], offset: Int, len: Int, index: Int): J = parentPath.wrap(super.visitExt(tag, bytes, offset, len, index))

  override def toString: String = parentPath.toString
}
