package ujson

import ujson.borer.json.JsonParser
import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

import scala.annotation.tailrec

object BorerInputParser {
  private val Sentinel = new Object()
  def transform0[T, Bytes](parser: ujson.borer.json.JsonParser[Bytes],
                           f: Visitor[_, T]): T = {
    val startCursor = parser.input.cursor.toInt

    var lastResult: T = null.asInstanceOf[T]
    object MyReceiver extends ujson.borer.Receiver{
      def onNull(): Unit = lastResult = f.visitNull(parser.valueIndex.toInt)
      def onBoolean(value: Boolean): Unit = {
        lastResult =
          if (value) f.visitTrue(parser.valueIndex.toInt)
          else f.visitFalse(parser.valueIndex.toInt)
      }

      def onInt(value: Int): Unit = lastResult = f.visitInt32(value, parser.valueIndex.toInt)
      def onLong(value: Long): Unit = lastResult = f.visitInt64(value, parser.valueIndex.toInt)
      def onFloat(value: Float): Unit = lastResult = f.visitFloat32(value, parser.valueIndex.toInt)
      def onDouble(value: Double): Unit = lastResult = f.visitFloat64(value, parser.valueIndex.toInt)
      def onNumberString(value: String): Unit = lastResult = f.visitFloat64String(value, parser.valueIndex.toInt)
      def onChars(buffer: Array[Char], length: Int): Unit = {
        lastResult = f.visitString(new scala.runtime.ArrayCharSequence(buffer, 0, length), parser.valueIndex.toInt)
      }
      def onArrayStart(): Unit = {}
      def onMapStart(): Unit = {}
      def onBreak(): Unit = {}
      def onEndOfInput(): Unit = {}
    }

    try {
      parser.pull(MyReceiver) match {
        case ujson.borer.DataItem.Break => Sentinel.asInstanceOf[T]
        case ujson.borer.DataItem.ArrayStart => transformArray0(parser, f)
        case ujson.borer.DataItem.MapStart => transformObject0(parser, f)
        case _ => lastResult
      }
    }catch{ case e: upickle.core.Abort =>
      // borer JsonParser doesn't track line/col numbers, so stub them out
      // with -1 for now. Also subtract 1 from startCursor because JsonParser's
      // input cursor seems to start from 1 instead of 0
      throw new upickle.core.AbortException(e.msg, startCursor - 1, -1, -1, e)
    }
  }

  private def transformObject0[Bytes, T](parser: JsonParser[Bytes],
                                         f: Visitor[_, T]) = {
    val objVisitor = f.visitObject(-1, parser.valueIndex.toInt).narrow
    transformObjectLoop(parser, objVisitor)
    objVisitor.visitEnd(parser.valueIndex.toInt)
  }

  @tailrec def transformObjectLoop[Bytes, T](parser: JsonParser[Bytes],
                                             objVisitor: ObjVisitor[Any, T]): Unit = {
    val k = transform0(parser, objVisitor.visitKey(parser.valueIndex.toInt)).asInstanceOf[AnyRef]
    if (k ne Sentinel) {
        objVisitor.visitKeyValue(k)
        val v = transform0(parser, objVisitor.subVisitor)
        objVisitor.visitValue(v, parser.valueIndex.toInt)
        transformObjectLoop(parser, objVisitor)
    }
  }

  private def transformArray0[Bytes, T](parser: JsonParser[Bytes],
                                        f: Visitor[_, T]) = {
    val arrVisitor = f.visitArray(-1, parser.valueIndex.toInt).narrow
    transformArrayLoop(parser, arrVisitor)
    arrVisitor.visitEnd(parser.valueIndex.toInt)
  }
  @tailrec def transformArrayLoop[Bytes, T](parser: JsonParser[Bytes],
                                            arrVisitor: ArrVisitor[Any, T]): Unit = {
    val v = transform0(parser, arrVisitor.subVisitor).asInstanceOf[AnyRef]
    if (v ne Sentinel) {
      arrVisitor.visitValue(v, parser.valueIndex.toInt)
      transformArrayLoop(parser, arrVisitor)
    }
  }

  def transform[Bytes: borer.ByteAccess, T](input: ujson.borer.Input[Bytes], f: Visitor[_, T]) = {
    val parser = new ujson.borer.json.JsonParser(
      input,
      new ujson.borer.json.JsonParser.Config{
        def readDecimalNumbersOnlyAsNumberStrings = true
        def maxStringLength = Int.MaxValue
        def maxNumberMantissaDigits = 1000000
        def maxNumberAbsExponent = Int.MaxValue
        def initialCharbufferSize = 1024
        def allowBufferCaching = true
      }
    )
    try transform0(parser, f)
    catch{
      case e: ujson.borer.Error.InvalidInputData[_] if e.getMessage.contains("but got end of input") =>
        throw new IncompleteParseException(e.getMessage, e)
      case e: ujson.borer.Error.InvalidInputData[_] =>
        throw new ParseException(e.getMessage, parser.valueIndex.toInt, -1, -1)

    }
  }
}
