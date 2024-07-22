package upack

import upickle.core.{ArrVisitor, ObjVisitor, LinkedHashMap, Visitor}

import upickle.core.compat._
import scala.collection.mutable

/**
  * In-memory representation of the MessagePack data model
  *
  test - https://msgpack.org/index.html
  *
  * Note that we do not model all the fine details of the MessagePack format
  * in this type; fixed and variable length strings/maps/arrays are all
  * modelled using the same [[Str]]/[[Obj]]/[[Arr]] types, and the various
  * sized integers are all collapsed into [[Int32]]/[[Int64]]/[[UInt64]]. The
  * appropriately sized versions are written out when the message is serialized
  * to bytes.
  */
sealed trait Msg extends Readable with geny.Writable{
  override def httpContentType = Some("application/octet-stream")
  def transform[T](f: Visitor[_, T]) = Msg.transform(this, f)

  /**
    * Returns the `String` value of this [[Msg]], fails if it is not
    * a [[upack.Str]]
    */
  def binary = this match{
    case Binary(value) => value
    case _ => throw Msg.InvalidData(this, "Expected ujson.Str")
  }
  /**
    * Returns the `String` value of this [[Msg]], fails if it is not
    * a [[upack.Str]]
    */
  def str = this match{
    case Str(value) => value
    case _ => throw Msg.InvalidData(this, "Expected ujson.Str")
  }
  /**
    * Returns the key/value map of this [[Msg]], fails if it is not
    * a [[upack.Obj]]
    */
  def obj = this match{
    case Obj(value) => value
    case _ => throw Msg.InvalidData(this, "Expected ujson.Obj")
  }
  /**
    * Returns the elements of this [[Msg]], fails if it is not
    * a [[upack.Arr]]
    */
  def arr = this match{
    case Arr(value) => value
    case _ => throw Msg.InvalidData(this, "Expected ujson.Arr")
  }
  /**
    * Returns the `Double` value of this [[Msg]], fails if it is not
    * a [[upack.Int32]], [[upack.Int64]] or [[upack.UInt64]]
    */
  def int32 = this match{
    case Int32(value) => value
    case Int64(value) => value.toInt
    case UInt64(value) => value.toInt
    case _ => throw Msg.InvalidData(this, "Expected ujson.Num")
  }
  /**
    * Returns the `Double` value of this [[Msg]], fails if it is not
    * a [[upack.Int32]], [[upack.Int64]] or [[upack.UInt64]]
    */
  def int64 = this match{
    case Int32(value) => value.toLong
    case Int64(value) => value
    case UInt64(value) => value
    case _ => throw Msg.InvalidData(this, "Expected ujson.Num")
  }
  /**
    * Returns the `Float` value of this [[Msg]], fails if it is not
    * a [[upack.Float32]] or [[upack.Float64]]
    */
  def float32 = this match{
    case Float32(value) => value
    case Float64(value) => value.toFloat
    case _ => throw Msg.InvalidData(this, "Expected ujson.Num")
  }
  /**
    * Returns the `Double` value of this [[Msg]], fails if it is not
    * a [[upack.Float32]] or [[upack.Float64]]
    */
  def float64 = this match{
    case Float32(value) => value.toDouble
    case Float64(value) => value
    case _ => throw Msg.InvalidData(this, "Expected ujson.Num")
  }
  /**
    * Returns the `Boolean` value of this [[Msg]], fails if it is not
    * a [[upack.Bool]]
    */
  def bool = this match{
    case Bool(value) => value
    case _ => throw Msg.InvalidData(this, "Expected ujson.Bool")
  }
  /**
    * Returns true if the value of this [[Msg]] is ujson.Null, false otherwise
    */
  def isNull = this match {
    case Null => true
    case _ => false
  }

  def writeBytesTo(out: java.io.OutputStream): Unit = {
    this.transform(new MsgPackWriter(out))
  }
}
case object Null extends Msg
case object True extends Bool{
  def value = true
}
case object False extends Bool{
  def value = false
}
case class Int32(value: Int) extends Msg
case class Int64(value: Long) extends Msg
case class UInt64(value: Long) extends Msg
case class Float32(value: Float) extends Msg
case class Float64(value: Double) extends Msg
case class Str(value: String) extends Msg
case class Binary(value: Array[Byte]) extends Msg
case class Arr(value: mutable.ArrayBuffer[Msg]) extends Msg
object Arr {
  def apply(items: Msg*): Arr = {
    val buf = new mutable.ArrayBuffer[Msg](items.size)
    items.foreach(item => buf += item)
    Arr(buf)
  }
}
case class Obj(value: LinkedHashMap[Msg, Msg]) extends Msg
object Obj{
  def apply(item: (Msg, Msg),
            items: (Msg, Msg)*): Obj = {
    val map = LinkedHashMap[Msg, Msg]()
    map.put(item._1, item._2)
    for (i <- items) map.put(i._1, i._2)
    Obj(map)
  }

  def apply(): Obj = Obj(LinkedHashMap[Msg, Msg]())
}
case class Ext(tag: Byte, data: Array[Byte]) extends Msg

sealed abstract class Bool extends Msg{
  def value: Boolean
}
object Bool{
  def apply(value: Boolean): Bool = if (value) True else False
  def unapply(bool: Bool): Some[Boolean] = Some(bool.value)
}

object Msg extends MsgVisitor[Msg, Msg]{
  /**
    * Thrown when uPickle tries to convert a JSON blob into a given data
    * structure but fails because part the blob is invalid
    *
    * @param data The section of the JSON blob that uPickle tried to convert.
    *             This could be the entire blob, or it could be some subtree.
    * @param msg Human-readable text saying what went wrong
    */
  case class InvalidData(data: Msg, msg: String)
    extends Exception(s"$msg (data: $data)")

  sealed trait Selector{
    def apply(x: Msg): Msg
    def update(x: Msg, y: Msg): Unit
  }
  object Selector{
    implicit class IntSelector(i: Int) extends Selector{
      def apply(x: Msg): Msg = x.arr(i)
      def update(x: Msg, y: Msg) = x.arr(i) = y
    }
    implicit class StringSelector(i: String) extends Selector{
      def apply(x: Msg): Msg = x.obj(Str(i))
      def update(x: Msg, y: Msg) = x.obj(Str(i)) = y
    }
    implicit class MsgSelector(i: Msg) extends Selector{
      def apply(x: Msg): Msg = x.obj(i)
      def update(x: Msg, y: Msg) = x.obj(i) = y
    }
  }
  def transform[T](j: Msg, f: Visitor[_, T]): T = {
    j match{
      case Null => f.visitNull(-1)
      case True => f.visitTrue(-1)
      case False => f.visitFalse(-1)

      case Int32(value) => f.visitInt32(value, -1)
      case Int64(value) => f.visitInt64(value, -1)

      case UInt64(value) => f.visitUInt64(value, -1)

      case Float32(value) => f.visitFloat32(value, -1)
      case Float64(value) => f.visitFloat64(value, -1)

      case Str(value) => f.visitString(value, -1)
      case Binary(value) => f.visitBinary(value, 0, value.length, -1)

      case Arr(items) => visitArrContents(f, items)

      case Obj(items) => visitObjContents(f, items)
      case Ext(tag, data) => f.visitExt(tag, data, 0, data.length, -1)

    }
  }

  private def visitObjContents[T](f: Visitor[_, T], items: LinkedHashMap[Msg, Msg]) = {
    val obj = f.visitObject(items.size, true, -1)
    val kvs = items.iterator
    while(kvs.hasNext){
      val kv = kvs.next()
      val k = kv._1
      val v = kv._2
      val keyVisitor = obj.visitKey(-1)
      obj.visitKeyValue(k.transform(keyVisitor))
      obj.narrow.visitValue(transform(v, obj.subVisitor), -1)
    }
    obj.visitEnd(-1)
  }

  private def visitArrContents[T](f: Visitor[_, T], items: mutable.ArrayBuffer[Msg]) = {
    val arr = f.visitArray(items.length, -1)
    var i = 0
    val itemsLength = items.length
    while (i < itemsLength) {
      arr.narrow.visitValue(transform(items(i), arr.subVisitor), -1)
      i += 1
    }
    arr.visitEnd(-1)
  }

  def visitArray(length: Int, index: Int) = new ArrVisitor[Msg, Msg] {
    val arr = mutable.ArrayBuffer[Msg]()
    def subVisitor = Msg
    def visitValue(v: Msg, index: Int): Unit = arr.append(v)
    def visitEnd(index: Int) = Arr(arr)
  }

  def visitObject(length: Int, jsonableKeys: Boolean, index: Int) = new ObjVisitor[Msg, Msg] {
    val map = LinkedHashMap[Msg, Msg]()
    var lastKey: Msg = null
    def subVisitor = Msg
    def visitValue(v: Msg, index: Int): Unit = map(lastKey) = v
    def visitEnd(index: Int) = Obj(map)
    def visitKey(index: Int) = Msg
    def visitKeyValue(s: Any): Unit = lastKey = s.asInstanceOf[Msg]
  }

  def visitNull(index: Int) = Null

  def visitFalse(index: Int) = False

  def visitTrue(index: Int) = True

  def visitFloat64(d: Double, index: Int) = Float64(d)

  def visitFloat32(d: Float, index: Int) = Float32(d)

  def visitInt32(i: Int, index: Int) = Int32(i)

  def visitInt64(i: Long, index: Int) = Int64(i)

  def visitUInt64(i: Long, index: Int) = UInt64(i)

  def visitString(s: CharSequence, index: Int) = Str(s.toString)

  def visitBinary(bytes: Array[Byte], offset: Int, len: Int, index: Int) =
    Binary(bytes.slice(offset, offset + len))

  def visitExt(tag: Byte, bytes: Array[Byte], offset: Int, len: Int, index: Int) =
    Ext(tag, bytes.slice(offset, offset + len))

  def visitChar(s: Char, index: Int) = Int32(s)

}
