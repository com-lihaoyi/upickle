package ujson



import upickle.core.Util
import upickle.core.{ObjArrVisitor, Visitor}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

sealed trait Value extends Transformable {
  def value: Any

  /**
    * Returns the `String` value of this [[Value]], fails if it is not
    * a [[Value.Str]]
    */
  def str = this match{
    case Value.Str(value) => value
    case _ => throw Value.InvalidData(this, "Expected ujson.Str")
  }
  /**
    * Returns the key/value map of this [[Value]], fails if it is not
    * a [[Value.Obj]]
    */
  def obj = this match{
    case Value.Obj(value) => value
    case _ => throw Value.InvalidData(this, "Expected ujson.Obj")
  }
  /**
    * Returns the elements of this [[Value]], fails if it is not
    * a [[Value.Arr]]
    */
  def arr = this match{
    case Value.Arr(value) => value
    case _ => throw Value.InvalidData(this, "Expected ujson.Arr")
  }
  /**
    * Returns the `Double` value of this [[Value]], fails if it is not
    * a [[Value.Num]]
    */
  def num = this match{
    case Value.Num(value) => value
    case _ => throw Value.InvalidData(this, "Expected ujson.Num")
  }
  /**
    * Returns the `Boolean` value of this [[Value]], fails if it is not
    * a [[Value.Bool]]
    */
  def bool = this match{
    case Value.Bool(value) => value
    case _ => throw Value.InvalidData(this, "Expected ujson.Bool")
  }
  /**
    * Returns true if the value of this [[Value]] is ujson.Null, false otherwise
    */
  def isNull = this match {
    case Value.Null => true
    case _ => false
  }

  def apply(s: Value.Selector): Value = s(this)
  def update(s: Value.Selector, v: Value): Unit = s(this) = v

  /**
    * Update a value in-place. Takes an `Int` or a `String`, through the
    * implicitly-constructe [[Value.Selector]] type.
    *
    * We cannot just overload `update` on `s: Int` and `s: String` because
    * of type inference problems in Scala 2.11.
    */
  def update(s: Value.Selector, f: Value => Value): Unit = s(this) = f(s(this))

  def transform[T](f: Visitor[_, T]) = Value.transform(this, f)
  override def toString = render()
  def render(indent: Int = -1, escapeUnicode: Boolean = false) = this.transform(StringRenderer(indent, escapeUnicode)).toString
}

/**
* A very small, very simple JSON AST that uPickle uses as part of its
* serialization process. A common standard between the Jawn AST (which
* we don't use so we don't pull in the bulk of Spire) and the Javascript
* JSON AST.
*/
object Value extends AstTransformer[Value]{
  type Value = ujson.Value
  sealed trait Selector{
    def apply(x: Value): Value
    def update(x: Value, y: Value): Unit
  }
  object Selector{
    implicit class IntSelector(i: Int) extends Selector{
      def apply(x: Value): Value = x.arr(i)
      def update(x: Value, y: Value) = x.arr(i) = y
    }
    implicit class StringSelector(i: String) extends Selector{
      def apply(x: Value): Value = x.obj(i)
      def update(x: Value, y: Value) = x.obj(i) = y
    }
  }

  @deprecated("use ujson.Str")
  val Str = ujson.Str
  @deprecated("use ujson.Str")
  type Str = ujson.Str
  @deprecated("use ujson.Obj")
  val Obj = ujson.Obj
  @deprecated("use ujson.Obj")
  type Obj = ujson.Obj
  @deprecated("use ujson.Arr")
  val Arr = ujson.Arr
  @deprecated("use ujson.Arr")
  type Arr = ujson.Arr
  @deprecated("use ujson.Num")
  val Num = ujson.Num
  @deprecated("use ujson.Num")
  type Num = ujson.Num
  @deprecated("use ujson.Bool")
  val Bool = ujson.Bool
  @deprecated("use ujson.Bool")
  type Bool = ujson.Bool
  @deprecated("use ujson.True")
  val True = ujson.True
  @deprecated("use ujson.False")
  val False = ujson.False
  @deprecated("use ujson.Null")
  val Null = ujson.Null
  implicit def JsonableSeq[T](items: TraversableOnce[T])
                             (implicit f: T => Value) = Arr.from(items.map(f))
  implicit def JsonableDict[T](items: TraversableOnce[(String, T)])
                              (implicit f: T => Value)= Obj.from(items.map(x => (x._1, f(x._2))))
  implicit def JsonableBoolean(i: Boolean) = if (i) Value.True else Value.False
  implicit def JsonableByte(i: Byte) = Num(i)
  implicit def JsonableShort(i: Short) = Num(i)
  implicit def JsonableInt(i: Int) = Num(i)
  implicit def JsonableLong(i: Long) = Str(i.toString)
  implicit def JsonableFloat(i: Float) = Num(i)
  implicit def JsonableDouble(i: Double) = Num(i)
  implicit def JsonableNull(i: Null) = Null
  implicit def JsonableString(s: CharSequence) = Str(s.toString)


  def transform[T](j: Value, f: Visitor[_, T]): T = {
    j match{
      case Value.Null => f.visitNull(-1)
      case Value.True => f.visitTrue(-1)
      case Value.False => f.visitFalse(-1)
      case Value.Str(s) => f.visitString(s, -1)
      case Value.Num(d) => f.visitFloat64(d, -1)
      case Value.Arr(items) => transformArray(f, items)
      case Value.Obj(items) => transformObject(f, items)
    }
  }

  def visitArray(length: Int, index: Int) = new AstArrVisitor[ArrayBuffer](xs => Value.Arr(xs))

  def visitObject(length: Int, index: Int) = new AstObjVisitor[mutable.LinkedHashMap[String, Value]](xs => Value.Obj(xs))

  def visitNull(index: Int) = Value.Null

  def visitFalse(index: Int) = Value.False

  def visitTrue(index: Int) = Value.True


  override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
    Value.Num(
      if (decIndex != -1 || expIndex != -1) s.toString.toDouble
      else Util.parseIntegralNum(s, decIndex, expIndex, index)
    )
  }

  override def visitFloat64(d: Double, index: Int) = Value.Num(d)

  def visitString(s: CharSequence, index: Int) = Value.Str(s.toString)

  /**
    * Thrown when uPickle tries to convert a JSON blob into a given data
    * structure but fails because part the blob is invalid
    *
    * @param data The section of the JSON blob that uPickle tried to convert.
    *             This could be the entire blob, or it could be some subtree.
    * @param msg Human-readable text saying what went wrong
    */
  case class InvalidData(data: Value, msg: String)
    extends Exception(s"$msg (data: $data)")
}

case class Str(value: String) extends Value
case class Obj(value: mutable.LinkedHashMap[String, Value]) extends Value

object Obj{
  implicit def from(items: TraversableOnce[(String, Value)]): Obj = {
    Obj(mutable.LinkedHashMap(items.toSeq:_*))
  }
  // Weird telescoped version of `apply(items: (String, Value)*)`, to avoid
  // type inference issues due to overloading the existing `apply` method
  // generated by the case class itself
  // https://github.com/lihaoyi/upickle/issues/230
  def apply[V <% Value](item: (String, V),
                        items: (String, Value)*): Obj = {
    val map = new mutable.LinkedHashMap[String, Value]()
    map.put(item._1, item._2)
    for (i <- items) map.put(i._1, i._2)
    Obj(map)
  }

  def apply(): Obj = Obj(new mutable.LinkedHashMap[String, Value]())
}
case class Arr(value: ArrayBuffer[Value]) extends Value

object Arr{
  implicit def from[T <% Value](items: TraversableOnce[T]): Arr =
    Arr(items.map(x => x: Value).to[mutable.ArrayBuffer])

  def apply(items: Value*): Arr = Arr(items.to[mutable.ArrayBuffer])
}
case class Num(value: Double) extends Value
sealed abstract class Bool extends Value{
  def value: Boolean
}
object Bool{
  def apply(value: Boolean): Bool = if (value) True else False
  def unapply(bool: Bool): Option[Boolean] = Some(bool.value)
}
case object False extends Bool{
  def value = false
}
case object True extends Bool{
  def value = true
}
case object Null extends Value{
  def value = null
}