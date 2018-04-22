package ujson


import ujson.util.Util

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

sealed trait Js extends Transformable {
  def value: Any


  /**
    * Returns the `String` value of this [[Js.Value]], fails if it is not
    * a [[Js.Str]]
    */
  def str = this match {
    case Js.Str(value) => value
    case _ => throw Js.InvalidData(this, "Expected Js.Str")
  }
  /**
    * Returns the key/value map of this [[Js.Value]], fails if it is not
    * a [[Js.Obj]]
    */
  def obj = this match {
    case Js.Obj(value) => value
    case _ => throw Js.InvalidData(this, "Expected Js.Obj")
  }
  /**
    * Returns the elements of this [[Js.Value]], fails if it is not
    * a [[Js.Arr]]
    */
  def arr = this match {
    case Js.Arr(value) => value
    case _ => throw Js.InvalidData(this, "Expected Js.Arr")
  }
  /**
    * Returns the `Double` value of this [[Js.Value]], fails if it is not
    * a [[Js.Num]]
    */
  def num = this match {
    case Js.Num(value) => value
    case _ => throw Js.InvalidData(this, "Expected Js.Num")
  }
  /**
    * Returns the `Boolean` value of this [[Js.Value]], fails if it is not
    * a [[Js.Bool]]
    */
  def bool = this match {
    case Js.Bool(value) => value
    case _ => throw Js.InvalidData(this, "Expected Js.Bool")
  }

  def apply(s: Js.Selector): Js.Value = s(this)
  def update(s: Js.Selector, v: Js.Value): Unit = s(this) = v

  /**
    * Update a value in-place. Takes an `Int` or a `String`, through the
    * implicitly-constructe [[Js.Selector]] type.
    *
    * We cannot just overload `update` on `s: Int` and `s: String` because
    * of type inference problems in Scala 2.11.
    */
  def update(s: Js.Selector, f: Js.Value => Js.Value): Unit = s(this) = f(s(this))

  def transform[T](f: ujson.Visitor[_, T]) = Js.transform(this, f)
  override def toString = render()
  def render(indent: Int = -1) = this.transform(StringRenderer(indent)).toString
}

/**
* A very small, very simple JSON AST that uPickle uses as part of its
* serialization process. A common standard between the Jawn AST (which
* we don't use so we don't pull in the bulk of Spire) and the Javascript
* JSON AST.
*/
object Js extends AstTransformer[Js] {
  sealed trait Selector {
    def apply(x: Js.Value): Js.Value
    def update(x: Js.Value, y: Js.Value): Unit
  }
  object Selector {
    implicit class IntSelector(i: Int) extends Selector {
      def apply(x: Js.Value): Js.Value = x.arr(i)
      def update(x: Js.Value, y: Js.Value) = x.arr(i) = y
    }
    implicit class StringSelector(i: String) extends Selector {
      def apply(x: Js.Value): Js.Value = x.obj(i)
      def update(x: Js.Value, y: Js.Value) = x.obj(i) = y
    }
  }

  case class Str(value: String) extends Value
  case class Obj(value: mutable.LinkedHashMap[String, Value]) extends Value
  object Obj {
    implicit def from(items: TraversableOnce[(String, Value)]): Obj = {
      Obj(mutable.LinkedHashMap(items.toSeq:_*))
    }
    def apply(items: (String, Value)*): Obj = Obj(mutable.LinkedHashMap(items:_*))
  }
  case class Arr(value: ArrayBuffer[Value]) extends Value

  object Arr {
    implicit def from[T <% Js.Value](items: TraversableOnce[T]): Arr =
      Arr(items.map(x => x: Js.Value).to[mutable.ArrayBuffer])

    def apply(items: Value*): Arr = Arr(items.to[mutable.ArrayBuffer])
  }
  case class Num(value: Double) extends Value
  sealed abstract class Bool extends Value {
    def value: Boolean
  }
  object Bool {
    def apply(value: Boolean): Bool = if (value) True else False
    def unapply(bool: Bool): Option[Boolean] = Some(bool.value)
  }
  case object False extends Bool {
    def value = false
  }
  case object True extends Bool {
    def value = true
  }
  case object Null extends Value {
    def value = null
  }

  implicit def JsonableSeq[T](items: TraversableOnce[T])
                             (implicit f: T => Js.Value) = Arr.from(items.map(f))
  implicit def JsonableDict[T](items: TraversableOnce[(String, T)])
                              (implicit f: T => Js.Value)= Obj.from(items.map(x => (x._1, f(x._2))))
  implicit def JsonableBoolean(i: Boolean) = if (i) Js.True else Js.False
  implicit def JsonableByte(i: Byte) = Num(i)
  implicit def JsonableShort(i: Short) = Num(i)
  implicit def JsonableInt(i: Int) = Num(i)
  implicit def JsonableLong(i: Long) = Str(i.toString)
  implicit def JsonableFloat(i: Float) = Num(i)
  implicit def JsonableDouble(i: Double) = Num(i)
  implicit def JsonableNull(i: Null) = Null
  implicit def JsonableString(s: CharSequence) = Str(s.toString)
  
  
  type Value = Js
  def transform[T](j: Js.Value, f: ujson.Visitor[_, T]): T = {
    j match {
      case Js.Null => f.visitNull(-1)
      case Js.True => f.visitTrue(-1)
      case Js.False => f.visitFalse(-1)
      case Js.Str(s) => f.visitString(s, -1)
      case Js.Num(d) => f.visitNumRaw(d, -1)
      case Js.Arr(items) => transformArray(f, items)
      case Js.Obj(items) => transformObject(f, items)
    }
  }

  def visitArray(index: Int) = new AstArrVisitor[ArrayBuffer](xs => Js.Arr(xs))

  def visitObject(index: Int) = new AstObjVisitor[mutable.LinkedHashMap[String, Js]](xs => Js.Obj(xs))

  def visitNull(index: Int) = Js.Null

  def visitFalse(index: Int) = Js.False

  def visitTrue(index: Int) = Js.True


  override def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
    Js.Num(
      if (decIndex != -1 || expIndex != -1) s.toString.toDouble
      else Util.parseIntegralNum(s, decIndex, expIndex, index)
    )
  }

  override def visitNumRaw(d: Double, index: Int) = Js.Num(d)

  def visitString(s: CharSequence, index: Int) = Js.Str(s.toString)

  /**
    * Thrown when uPickle tries to convert a JSON blob into a given data
    * structure but fails because part the blob is invalid
    *
    * @param data The section of the JSON blob that uPickle tried to convert.
    *             This could be the entire blob, or it could be some subtree.
    * @param msg Human-readable text saying what went wrong
    */
  case class InvalidData(data: Js.Value, msg: String) extends Exception(s"$msg (data: $data)")
}

