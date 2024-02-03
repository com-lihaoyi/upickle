package upickle.core

import upickle.core.ParseUtils.reject
import scala.collection.mutable

/**
  * A reified version of [[Visitor]], allowing visitor method calls to be buffered up,
  * stored somewhere, and replayed later.
  */
sealed trait BufferedValue {
  def index: Int
}

object BufferedValue extends Transformer[BufferedValue]{
  
  case class Str(index: Int, value0: java.lang.CharSequence) extends BufferedValue
  case class Obj(index: Int, jsonableKeys: Boolean, value0: mutable.ArrayBuffer[(BufferedValue, BufferedValue)]) extends BufferedValue
  case class Arr(index: Int, value: mutable.ArrayBuffer[BufferedValue]) extends BufferedValue
  case class Num(index: Int, s: CharSequence, decIndex: Int, expIndex: Int) extends BufferedValue
  case class NumRaw(index: Int, d: Double) extends BufferedValue
  case class False(index: Int) extends BufferedValue{
    def value = false
  }
  case class True(index: Int) extends BufferedValue{
    def value = true
  }
  case class Null(index: Int) extends BufferedValue{
    def value = null
  }

  case class Binary(bytes: Array[Byte], offset: Int, len: Int, index: Int) extends BufferedValue
  case class Char(s: scala.Char, index: Int) extends BufferedValue
  case class Ext(tag: Byte, bytes: Array[Byte], offset: Int, len: Int, index: Int) extends BufferedValue
  case class Float32(d: Float, index: Int) extends BufferedValue
  case class Float64String(s: String, index: Int) extends BufferedValue
  case class Int32(i: Int, index: Int) extends BufferedValue
  case class Int64(i: Long, index: Int) extends BufferedValue
  case class UInt64(i: Long, index: Int) extends BufferedValue

  def transform[T](j: BufferedValue, f: Visitor[_, T]): T = try{
    j match{
      case BufferedValue.Null(i) => f.visitNull(i)
      case BufferedValue.True(i) => f.visitTrue(i)
      case BufferedValue.False(i) => f.visitFalse(i)
      case BufferedValue.Str(i, s) => f.visitString(s, i)
      case BufferedValue.Num(i, s, d, e) => f.visitFloat64StringParts(s, d, e, i)
      case BufferedValue.NumRaw(i, d) => f.visitFloat64(d, i)
      case BufferedValue.Arr(i, items) =>
        val ctx = f.visitArray(items.length, i).narrow
        for(item <- items) try ctx.visitValue(transform(item, ctx.subVisitor), item.index) catch reject(item.index)
        ctx.visitEnd(i)

      case BufferedValue.Obj(i, jsonableKeys, items) =>
        val ctx = f.visitObject(items.length, jsonableKeys, i).narrow
        for((k, item) <- items) {
          val keyVisitor = try ctx.visitKey(i) catch reject(i)
          val key = transform(k, keyVisitor)
          ctx.visitKeyValue(key)
          try ctx.visitValue(transform(item, ctx.subVisitor), item.index)
          catch reject(item.index)
        }
        ctx.visitEnd(i)
      case BufferedValue.Binary(bytes, offset, len, index) => f.visitBinary(bytes, offset, len, index)
      case BufferedValue.Char(s, index) => f.visitChar(s, index)
      case BufferedValue.Ext(tag, bytes, offset, len, index) => f.visitExt(tag, bytes, offset, len, index)
      case BufferedValue.Float32(d, index) => f.visitFloat32(d, index)
      case BufferedValue.Float64String(s, index) => f.visitFloat64String(s, index)
      case BufferedValue.Int32(i, index) => f.visitInt32(i, index)
      case BufferedValue.Int64(i, index) => f.visitInt64(i, index)
      case BufferedValue.UInt64(i, index) => f.visitUInt64(i, index)
    }
  } catch reject(j.index)


  object Builder extends Visitor[BufferedValue, BufferedValue]{
    def visitArray(length: Int, i: Int) = new ArrVisitor[BufferedValue, BufferedValue.Arr] {
      val out = mutable.ArrayBuffer.empty[BufferedValue]
      def subVisitor = Builder
      def visitValue(v: BufferedValue, index: Int): Unit = {
        out.append(v)
      }
      def visitEnd(index: Int): BufferedValue.Arr = BufferedValue.Arr(i, out)
    }

    def visitObject(length: Int, jsonableKeys: Boolean, i: Int) = new ObjVisitor[BufferedValue, BufferedValue.Obj] {
      val out = mutable.ArrayBuffer.empty[(BufferedValue, BufferedValue)]
      var currentKey: BufferedValue = _
      def subVisitor = Builder
      def visitKey(index: Int) = BufferedValue.Builder
      def visitKeyValue(s: Any): Unit = currentKey = s.asInstanceOf[BufferedValue]
      def visitValue(v: BufferedValue, index: Int): Unit = {
        out.append((currentKey, v))
      }
      def visitEnd(index: Int): BufferedValue.Obj = BufferedValue.Obj(i, jsonableKeys, out)
    }

    def visitNull(i: Int) = BufferedValue.Null(i)

    def visitFalse(i: Int) = BufferedValue.False(i)

    def visitTrue(i: Int) = BufferedValue.True(i)

    def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, i: Int) = BufferedValue.Num(i, s, decIndex, expIndex)
    override def visitFloat64(d: Double, i: Int) = BufferedValue.NumRaw(i, d)

    def visitString(s: CharSequence, i: Int) = BufferedValue.Str(i, s)

    def visitBinary(bytes: Array[Byte], offset: Int, len: Int, index: Int) = BufferedValue. Binary(bytes, offset, len, index)
    def visitChar(s: scala.Char, index: Int) = BufferedValue. Char(s, index)
    def visitExt(tag: Byte, bytes: Array[Byte], offset: Int, len: Int, index: Int) = BufferedValue. Ext(tag, bytes, offset, len, index)
    def visitFloat32(d: Float, index: Int) = BufferedValue. Float32(d, index)
    def visitFloat64String(s: String, index: Int) = BufferedValue. Float64String(s, index)
    def visitInt32(i: Int, index: Int) = BufferedValue. Int32(i, index)
    def visitInt64(i: Long, index: Int) = BufferedValue. Int64(i, index)
    def visitUInt64(i: Long, index: Int) = BufferedValue. UInt64(i, index)
  }
}
