package upack

import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

sealed trait Msg extends Transformable{
  def transform[T](f: Visitor[_, T]) = Msg.transform(this, f)
}
case object Null extends Msg
case object True extends Msg
case object False extends Msg
case class Int8(value: Byte) extends Msg
case class Int16(value: Short) extends Msg
case class Int32(value: Int) extends Msg
case class Int64(value: Long) extends Msg
case class UInt8(value: Byte) extends Msg
case class UInt16(value: Short) extends Msg
case class UInt32(value: Int) extends Msg
case class UInt64(value: Long) extends Msg
case class Float32(value: Float) extends Msg
case class Float64(value: Double) extends Msg
case class Str(value: String) extends Msg
case class Binary(value: Array[Byte]) extends Msg
case class Arr(value: mutable.ArrayBuffer[Msg]) extends Msg
case class Obj(value: mutable.Map[Msg, Msg]) extends Msg
case class Ext(tag: Byte, data: Array[Byte]) extends Msg

object Msg extends Visitor[Msg, Msg]{

  def transform[T](j: Msg, f: Visitor[_, T]): T = {
    j match{
      case Null => f.visitNull(-1)
      case True => f.visitTrue(-1)
      case False => f.visitFalse(-1)

      case Int8(value) => f.visitInt8(value, -1)
      case Int16(value) => f.visitInt16(value, -1)
      case Int32(value) => f.visitInt32(value, -1)
      case Int64(value) => f.visitInt64(value, -1)

      case UInt8(value) => f.visitUInt8(value, -1)
      case UInt16(value) => f.visitUInt16(value, -1)
      case UInt32(value) => f.visitUInt32(value, -1)
      case UInt64(value) => f.visitUInt64(value, -1)

      case Float32(value) => f.visitFloat32(value, -1)
      case Float64(value) => f.visitFloat64(value, -1)

      case Str(value) => f.visitString(value, -1)
      case Binary(value) => f.visitBin(value, 0, value.length, -1)

      case Arr(items) =>
        val arr = f.visitArray(items.length, -1)
        for(i <- items){
          arr.narrow.visitValue(transform(i, arr.subVisitor), -1)
        }
        arr.visitEnd(-1)

      case Obj(items) =>
        val obj = f.visitObject(items.size, -1)
        for((k, v) <- items){
          obj.visitKey(k.asInstanceOf[Str].value, -1)
          obj.narrow.visitValue(transform(v, obj.subVisitor), -1)
        }
        obj.visitEnd(-1)

    }
  }
  def visitArray(length: Int, index: Int) = new ArrVisitor[Msg, Msg] {
    val arr = ArrayBuffer[Msg]()
    def subVisitor = Msg
    def visitValue(v: Msg, index: Int): Unit = arr.append(v)
    def visitEnd(index: Int) = Arr(arr)
  }

  def visitObject(length: Int, index: Int) = new ObjVisitor[Msg, Msg] {
    val map = mutable.Map[Msg, Msg]()
    var lastKey = ""
    def subVisitor = Msg
    def visitValue(v: Msg, index: Int): Unit = map(Str(lastKey)) = v
    def visitEnd(index: Int) = Obj(map)
    def visitKey(s: CharSequence, index: Int): Unit = lastKey = s.toString
  }

  def visitNull(index: Int) = Null

  def visitFalse(index: Int) = False

  def visitTrue(index: Int) = True

  def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = ???

  def visitFloat64(d: Double, index: Int) = Float64(d)

  def visitFloat32(d: Float, index: Int) = Float32(d)

  def visitInt8(i: Byte, index: Int) = Int8(i)
  def visitInt16(i: Short, index: Int) = Int16(i)
  def visitInt32(i: Int, index: Int) = Int32(i)

  def visitUInt8(i: Byte, index: Int) = UInt8(i)
  def visitUInt16(i: Short, index: Int) = UInt16(i)
  def visitUInt32(i: Int, index: Int) = UInt32(i)

  def visitInt64(i: Long, index: Int) = Int64(i)

  def visitUInt64(i: Long, index: Int) = UInt64(i)

  def visitFloat64String(s: Predef.String, index: Int) = ???

  def visitString(s: CharSequence, index: Int) = Str(s.toString)

  def visitBin(bytes: Array[Byte], offset: Int, len: Int, index: Int) =
    Binary(bytes.slice(offset, offset + len))

  def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int) = ???

  def visitExt(tag: Byte, bytes: Array[Byte], offset: Int, len: Int, index: Int) =
    Ext(tag, bytes.slice(offset, offset + len))
}