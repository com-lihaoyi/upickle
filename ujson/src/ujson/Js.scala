package ujson

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

sealed trait Js extends Transformable{
  def value: Any


  /**
    * Returns the `String` value of this [[Js.Value]], fails if it is not
    * a [[Js.Str]]
    */
  def str = this match{
    case Js.Str(value) => value
    case _ => throw Js.InvalidData(this, "Expected Js.Str")
  }
  /**
    * Returns the key/value map of this [[Js.Value]], fails if it is not
    * a [[Js.Obj]]
    */
  def obj = this match{
    case Js.Obj(value) => value
    case _ => throw Js.InvalidData(this, "Expected Js.Obj")
  }
  /**
    * Returns the elements of this [[Js.Value]], fails if it is not
    * a [[Js.Arr]]
    */
  def arr = this match{
    case Js.Arr(value) => value
    case _ => throw Js.InvalidData(this, "Expected Js.Arr")
  }
  /**
    * Returns the `Double` value of this [[Js.Value]], fails if it is not
    * a [[Js.Num]]
    */
  def num = this match{
    case Js.Num(value) => value
    case _ => throw Js.InvalidData(this, "Expected Js.Num")
  }

  /**
    * Looks up the [[Js.Value]] as a [[Js.Arr]] using an index, throws
    * otherwise if it's not a [[Js.Arr]]
    */
  def apply(i: Int): Js.Value = this.arr(i)
  def update(i: Int, v: Js.Value): Unit = this.arr(i) = v
  /**
    * Looks up the [[Js.Value]] as a [[Js.Obj]] using an index, throws
    * otherwise if it's not a [[Js.Obj]]
    */
  def apply(s: java.lang.String): Js.Value = this.obj(s)
  def update(s: String, v: Js.Value): Unit = this.obj(s) = v

  def transform[T](f: ujson.Visitor[_, T]) = Js.transform(this, f)
  override def toString = this.transform(StringRenderer(4)).toString
}

/**
* A very small, very simple JSON AST that uPickle uses as part of its
* serialization process. A common standard between the Jawn AST (which
* we don't use so we don't pull in the bulk of Spire) and the Javascript
* JSON AST.
*/
object Js extends AstTransformer[Js]{

  case class Str(value: String) extends Value
  case class Obj(value: mutable.Map[String, Value]) extends Value
  object Obj{
    def apply(items: (String, Value)*): Obj = Obj(mutable.Map(items:_*))
  }
  case class Arr(value: ArrayBuffer[Value]) extends Value
  object Arr{
    def apply(items: Value*): Arr = Arr(items.to[mutable.ArrayBuffer])
  }
  case class Num(value: Double) extends Value
  case object False extends Value{
    def value = false
  }
  case object True extends Value{
    def value = true
  }
  case object Null extends Value{
    def value = null
  }

  type Value = Js
  def transform[T](j: Js.Value, f: ujson.Visitor[_, T]): T = {
    j match{
      case Js.Null => f.visitNull(-1)
      case Js.True => f.visitTrue(-1)
      case Js.False => f.visitFalse(-1)
      case Js.Str(s) => f.visitString(s, -1)
      case Js.Num(d) => f.visitNumRaw(d, -1)
      case Js.Arr(items) => transformArray(f, items)
      case Js.Obj(items) => transformObject(f, items)
    }
  }

  object Builder extends Builder{
    def visitArray(index: Int) = new AstArrVisitor(xs => Js.Arr(xs))

    def visitObject(index: Int) = new AstObjVisitor(xs => Js.Obj(xs:_*))

    def visitNull(index: Int) = Js.Null

    def visitFalse(index: Int) = Js.False

    def visitTrue(index: Int) = Js.True

    def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      Js.Num(s.toString.toDouble)
    }
    override def visitNumRaw(d: Double, index: Int) = Js.Num(d)

    def visitString(s: CharSequence, index: Int) = Js.Str(s.toString)
  }

  /**
    * Thrown when uPickle tries to convert a JSON blob into a given data
    * structure but fails because part the blob is invalid
    *
    * @param data The section of the JSON blob that uPickle tried to convert.
    *             This could be the entire blob, or it could be some subtree.
    * @param msg Human-readable text saying what went wrong
    */
  case class InvalidData(data: Js.Value, msg: String)
    extends Exception(s"$msg (data: $data)")
}

