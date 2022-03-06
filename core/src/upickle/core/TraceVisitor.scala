package upickle.core

import upickle.core.TraceVisitor._

import scala.annotation.tailrec
import scala.util.control.NoStackTrace

/**
  * Adds a JSON Path to exceptions thrown by the delegate Visitor.
  *
  * Useful for debugging failures.
  * Adds ~10% overhead depending on the parser.
  *
  * @see https://goessner.net/articles/JsonPath/
  */
object TraceVisitor {
  def withTrace[T, J](trace: Boolean, v: Visitor[T, J])(f: Visitor[T, J] => J): J = {
    if (!trace) f(v)
    else{
      val wrapper = new upickle.core.TraceVisitor.Wrapper[T, J]()
      try f(wrapper.visitor(v))
      catch{case e => throw new upickle.core.TraceVisitor.TraceException(wrapper.lastHasPath.toString, e) }
    }
  }

  /**
    * JSON Path indicating where the problem occurred.
    * Added as a suppressed exception.
    */
  class TraceException(val jsonPath: String, cause: Throwable) extends Exception(jsonPath, cause) with NoStackTrace

  /**
    * Internally, the paths form a linked list back to the root by the visitors themselves.
    * Compared to something like a List[String] or List[Object], this does not require
    * extra String allocation or boxing unless we actually ask for the path.
    */
  trait HasPath {

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
      * @return the full JSONPath
      */
    def path: String = "$" + components.iterator.flatMap(_.pathComponent).map("[" + _ + "]").mkString

    override def toString: String = path
  }

  object RootHasPath extends HasPath {

    override def pathComponent: Option[String] = None

    override def parent: Option[HasPath] = None
  }
  class Wrapper[T, J]{
    def visitor(delegate: Visitor[T, J]): TraceVisitor[T, J] = new TraceVisitor(delegate, TraceVisitor.RootHasPath, this)
    var lastHasPath: HasPath = TraceVisitor.RootHasPath
  }
}


class TraceVisitor[T, J](
  protected val delegate: Visitor[T, J],
  parentPath: HasPath,
  wrapper: TraceVisitor.Wrapper[T, J]
) extends Visitor.Delegate[T, J](delegate) {
  wrapper.lastHasPath = parentPath
  override def visitObject(length: Int, jsonableKeys: Boolean, index: Int): ObjVisitor[T, J] = {
    val objVisitor = super.visitObject(length, jsonableKeys, index)
    new ObjVisitor[T, J] with HasPath {
      private var key: String = _

      override def visitKey(index: Int): Visitor[_, _] = new TraceVisitor[Nothing, Any](
        objVisitor.visitKey(index),
        this,
        wrapper.asInstanceOf[TraceVisitor.Wrapper[Nothing, Any]]
      ) {
        override def visitString(s: CharSequence, index: Int): Any = {
          key = s.toString
          this.delegate.visitString(key, index)
        }
      }

      override def visitKeyValue(v: Any): Unit = {
        if (key == null) key = "?"
        objVisitor.visitKeyValue(v)
      }

      override def subVisitor: Visitor[Nothing, Any] = {
        new TraceVisitor(objVisitor.subVisitor.asInstanceOf[Visitor[T, J]], this, wrapper)
      }

      override def visitValue(v: T, index: Int): Unit = {
        key = null // reset before visitEnd.
        objVisitor.visitValue(v, index)
      }

      override def visitEnd(index: Int): J = {
        wrapper.lastHasPath = parentPath
        objVisitor.visitEnd(index)
      }

      override def pathComponent: Option[String] = Option(key).map("'" + _.replaceAllLiterally("'", "\\'") + "'")

      override def parent: Option[HasPath] = Some(TraceVisitor.this.parentPath)
    }
  }

  override def visitArray(length: Int, index: Int): ArrVisitor[T, J] = {
    val arrVisitor = super.visitArray(length, index)
    new ArrVisitor[T, J] with HasPath {
      private var i = 0
      wrapper.lastHasPath = this
      override def subVisitor: Visitor[Nothing, Any] = {

        new TraceVisitor(arrVisitor.subVisitor.asInstanceOf[Visitor[T, J]], this, wrapper)
      }

      override def visitValue(v: T, index: Int): Unit = {
        arrVisitor.visitValue(v, index)
        i += 1
      }

      override def visitEnd(index: Int): J = {
        wrapper.lastHasPath = parentPath
        arrVisitor.visitEnd(index)
      }

      override def pathComponent: Option[String] = Some(i.toString)

      override def parent: Option[HasPath] = Some(TraceVisitor.this.parentPath)
    }
  }

  override def toString: String = parentPath.toString
}
