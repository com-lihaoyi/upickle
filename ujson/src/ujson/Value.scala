package ujson

import upickle.core.{LinkedHashMap, ObjArrVisitor, ParseUtils, Visitor}
import upickle.core.compat._

import scala.language.implicitConversions
import scala.collection.mutable

sealed trait Value extends Readable with geny.Writable{
  override def httpContentType = Some("application/json")
  def value: Any

  /**
    * Returns the `String` value of this [[Value]], fails if it is not
    * a [[ujson.Str]]
    */
  def str = this match{
    case ujson.Str(value) => value
    case _ => throw Value.InvalidData(this, "Expected ujson.Str")
  }

  /**
    * Returns an Optional `String` value of this [[Value]] in case this [[Value]] is a 'String'.
    */
  def strOpt = this match{
    case Str(value) => Some(value)
    case _ => None
  }

  /**
    * Returns the key/value map of this [[Value]], fails if it is not
    * a [[ujson.Obj]]
    */
  def obj = this match{
    case ujson.Obj(value) => value
    case _ => throw Value.InvalidData(this, "Expected ujson.Obj")
  }
  /**
    * Returns an Optional key/value map of this [[Value]] in case this [[Value]] is a 'Obj'.
    */
  def objOpt = this match{
    case Obj(value) => Some(value)
    case _ => None
  }
  /**
    * Returns the elements of this [[Value]], fails if it is not
    * a [[ujson.Arr]]
    */
  def arr = this match{
    case ujson.Arr(value) => value
    case _ => throw Value.InvalidData(this, "Expected ujson.Arr")
  }
  /**
    * Returns The optional elements of this [[Value]] in case this [[Value]] is a 'Arr'.
    */
  def arrOpt = this match{
    case Arr(value) => Some(value)
    case _ => None
  }
  /**
    * Returns the `Double` value of this [[Value]], fails if it is not
    * a [[ujson.Num]]
    */
  def num = this match{
    case ujson.Num(value) => value
    case _ => throw Value.InvalidData(this, "Expected ujson.Num")
  }
  /**
    * Returns an Option[Double] in case this [[Value]] is a 'Num'.
    */
  def numOpt = this match{
    case Num(value) => Some(value)
    case _ => None
  }
  /**
    * Returns the `Boolean` value of this [[Value]], fails if it is not
    * a [[ujson.Bool]]
    */
  def bool = this match{
    case ujson.Bool(value) => value
    case _ => throw Value.InvalidData(this, "Expected ujson.Bool")
  }
  /**
    * Returns an Optional `Boolean` value of this [[Value]] in case this [[Value]] is a 'Bool'.
    */
  def boolOpt = this match{
    case Bool(value) => Some(value)
    case _ => None
  }
  /**
    * Returns true if the value of this [[Value]] is ujson.Null, false otherwise
    */
  def isNull = this match {
    case ujson.Null => true
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

  def writeBytesTo(out: java.io.OutputStream, indent: Int = -1, escapeUnicode: Boolean = false): Unit = {
    this.transform(new ujson.BaseByteRenderer(out, indent, escapeUnicode))
  }
  def writeBytesTo(out: java.io.OutputStream): Unit = writeBytesTo(out, -1, false)
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

  implicit def JsonableSeq[T](items: IterableOnce[T])
                             (implicit f: T => Value): Arr = Arr.from(toIterator(items).map(f))
  implicit def JsonableDict[T](items: IterableOnce[(String, T)])
                              (implicit f: T => Value): Obj = Obj.from(toIterator(items).map(x => (x._1, f(x._2))))
  implicit def JsonableBoolean(i: Boolean): Bool = if (i) ujson.True else ujson.False
  implicit def JsonableByte(i: Byte): Num = Num(i)
  implicit def JsonableShort(i: Short): Num = Num(i)
  implicit def JsonableInt(i: Int): Num = Num(i)
  implicit def JsonableLong(i: Long): Str = Str(i.toString)
  implicit def JsonableFloat(i: Float): Num = Num(i)
  implicit def JsonableDouble(i: Double): Num = Num(i)
  implicit def JsonableNull(i: Null): Null.type = Null
  implicit def JsonableString(s: CharSequence): Str = Str(s.toString)


  def transform[T](j: Value, f: Visitor[_, T]): T = {
    j match{
      case ujson.Null => f.visitNull(-1)
      case ujson.True => f.visitTrue(-1)
      case ujson.False => f.visitFalse(-1)
      case ujson.Str(s) => f.visitString(s, -1)
      case ujson.Num(d) => f.visitFloat64(d, -1)
      case ujson.Arr(items) => transformArray(f, items)
      case ujson.Obj(items) => transformObject(f, items)
    }
  }

  def visitArray(length: Int, index: Int) = new AstArrVisitor[mutable.ArrayBuffer](xs => ujson.Arr(xs))

  def visitJsonableObject(length: Int, index: Int) = new AstObjVisitor[LinkedHashMap[String, Value]](xs => ujson.Obj(xs))

  def visitNull(index: Int) = ujson.Null

  def visitFalse(index: Int) = ujson.False

  def visitTrue(index: Int) = True


  override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
    ujson.Num(
      if (decIndex != -1 || expIndex != -1) s.toString.toDouble
      else ParseUtils.parseIntegralNum(s, decIndex, expIndex, index).toDouble
    )
  }

  override def visitFloat64(d: Double, index: Int) = ujson.Num(d)

  def visitString(s: CharSequence, index: Int) = ujson.Str(s.toString)

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
case class Obj(value: LinkedHashMap[String, Value]) extends Value
object Obj{
  implicit def from(items: IterableOnce[(String, Value)]): Obj = {
    Obj(LinkedHashMap(items))
  }

  // Weird telescoped version of `apply(items: (String, Value)*)`, to avoid
  // type inference issues due to overloading the existing `apply` method
  // generated by the case class itself
  // https://github.com/lihaoyi/upickle/issues/230
  def apply[V](item: (String, V),
               items: (String, Value)*)(implicit conv: V => Value): Obj = {
    val map = LinkedHashMap[String, Value]()
    map.put(item._1, conv(item._2))
    for (i <- items) map.put(i._1, i._2)
    Obj(map)
  }
  def apply(): Obj = Obj(LinkedHashMap[String, Value]())
}
case class Arr(value: mutable.ArrayBuffer[Value]) extends Value

object Arr{
  implicit def from[T](items: IterableOnce[T])(implicit conv: T => Value): Arr = {
    val buf = new mutable.ArrayBuffer[Value]()
    toIterator(items).foreach{ item =>
      buf += (conv(item): Value)
    }
    Arr(buf)
  }

  def apply(items: Value*): Arr = {
    val buf = new mutable.ArrayBuffer[Value](items.length)
    items.foreach{ item =>
      buf += item
    }
    Arr(buf)
  }
}
case class Num(value: Double) extends Value
sealed abstract class Bool extends Value{
  def value: Boolean
}
object Bool{
  def apply(value: Boolean): Bool = if (value) True else False
  def unapply(bool: Bool): Some[Boolean] = Some(bool.value)
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
