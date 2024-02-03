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
  def valueToSortKey(b: BufferedValue): String = b match{
    case BufferedValue.Null(i) => "00"
    case BufferedValue.True(i) => "01" + "true"
    case BufferedValue.False(i) => "02" + "false"
    case BufferedValue.Str(s, i) => "03" +  s.toString
    case BufferedValue.Num(s, _, _, i) => "04" + s.toString
    case BufferedValue.Char(c, i) => "05" + c.toString
    case BufferedValue.Binary(bytes, o, l, _) => "06" + new String(bytes, o, l)
    case BufferedValue.Ext(tag, bytes, o, l, i) => "07" + tag.toString + new String(bytes, o, l)
    case BufferedValue.Float32(f, i) => "08" + f.toString
    case BufferedValue.Float64String(s, i) => "09" + s
    case BufferedValue.Int32(n, i) => "10" + n.toString
    case BufferedValue.Int64(n, i) => "11" + n.toString
    case BufferedValue.NumRaw(d, i) => "12" + d.toString
    case BufferedValue.UInt64(n, i) => "13" + n.toString
    case BufferedValue.Arr(vs, i) => "14" + vs.map(valueToSortKey).mkString
    case BufferedValue.Obj(kvs, _, i) => "15" + kvs.map{case (k, v) => valueToSortKey(k) + valueToSortKey(v)}.mkString
  }

  def maybeSortKeysTransform[T, V](tr: Transformer[T],
                                   t: T,
                                   sortKeys: Boolean,
                                   f: Visitor[_, V]): V = {
    def rec(x: BufferedValue): Unit = {
      x match {
        case BufferedValue.Arr(items, i) => items.map(rec)
        case BufferedValue.Obj(items, jsonableKeys, i) =>
          upickle.core.compat.SortInPlace[(BufferedValue, BufferedValue), String](items) {
            case (k, v) => valueToSortKey(k)
          }
          items.foreach { case (c, v) => (c, rec(v)) }
        case v =>
      }
    }

    if (sortKeys) {
      val buffered = tr.transform(t, BufferedValue.Builder)
      rec(buffered)
      BufferedValue.transform(buffered, f)
    } else {
      tr.transform(t, f)
    }
  }

  case class Str(value0: java.lang.CharSequence, index: Int) extends BufferedValue
  case class Obj(value0: mutable.ArrayBuffer[(BufferedValue, BufferedValue)], jsonableKeys: Boolean, index: Int) extends BufferedValue
  case class Arr(value: mutable.ArrayBuffer[BufferedValue], index: Int) extends BufferedValue
  case class Num(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) extends BufferedValue
  case class NumRaw(d: Double, index: Int) extends BufferedValue
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
      case BufferedValue.Str(s, i) => f.visitString(s, i)
      case BufferedValue.Num(s, d, e, i) => f.visitFloat64StringParts(s, d, e, i)
      case BufferedValue.NumRaw(d, i) => f.visitFloat64(d, i)
      case BufferedValue.Arr(items, i) =>
        val ctx = f.visitArray(items.length, i).narrow
        for(item <- items) try ctx.visitValue(transform(item, ctx.subVisitor), item.index) catch reject(item.index)
        ctx.visitEnd(i)

      case BufferedValue.Obj(items, jsonableKeys, i) =>
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
      def visitEnd(index: Int): BufferedValue.Arr = BufferedValue.Arr(out, i)
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
      def visitEnd(index: Int): BufferedValue.Obj = BufferedValue.Obj(out, jsonableKeys, i)
    }

    def visitNull(i: Int) = BufferedValue.Null(i)

    def visitFalse(i: Int) = BufferedValue.False(i)

    def visitTrue(i: Int) = BufferedValue.True(i)

    def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, i: Int) = BufferedValue.Num(s, decIndex, expIndex, i)
    override def visitFloat64(d: Double, i: Int) = BufferedValue.NumRaw(d, i)

    def visitString(s: CharSequence, i: Int) = BufferedValue.Str(s, i)

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
