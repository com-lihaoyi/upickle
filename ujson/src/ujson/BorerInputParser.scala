package ujson

import upickle.core.Visitor

object BorerInputParser {
  def transform0[T, Bytes](parser: ujson.borer.json.JsonParser[Bytes],
                           f0: Visitor[_, T],
                           cursorInt: => Int): T = {
    var lastResult: T = null.asInstanceOf[T]
    var visitorStack = List.empty[upickle.core.ObjArrVisitor[T, T]]
    var fStack = List(f0)
    def f = fStack.head
    var isKey = false
    def storeResult(t: T) = {
      lastResult = t
      visitorStack match {
        case head :: rest =>
          if (isKey) {
            head.asInstanceOf[upickle.core.ObjVisitor[T, T]].visitKeyValue(lastResult)
            isKey = false
            fStack = head.subVisitor.asInstanceOf[Visitor[_, T]] :: fStack.tail
          } else {
            head.visitValue(lastResult, cursorInt)
            head match {
              case head: upickle.core.ObjVisitor[T, T] =>
                isKey = true
                fStack = head.visitKey(cursorInt).asInstanceOf[Visitor[_, T]] :: fStack.tail
              case _ =>
                fStack = head.subVisitor.asInstanceOf[Visitor[_, T]] :: fStack.tail
            }
          }
        case _ => //donothing
      }
    }

    object MyReceiver extends ujson.borer.Receiver{
      def onNull(): Unit = storeResult(f.visitNull(cursorInt))
      def onBoolean(value: Boolean): Unit = {
        storeResult(
          if (value) f.visitTrue(cursorInt)
          else f.visitFalse(cursorInt)
        )
      }

      def onInt(value: Int): Unit = {
        storeResult(f.visitInt32(value, cursorInt))
      }
      def onLong(value: Long): Unit = storeResult(f.visitInt64(value, cursorInt))
      def onFloat(value: Float): Unit = storeResult(f.visitFloat32(value, cursorInt))
      def onDouble(value: Double): Unit = storeResult(f.visitFloat64(value, cursorInt))
      def onNumberString(value: String): Unit = storeResult(f.visitFloat64String(value, cursorInt))
      def onChars(buffer: Array[Char], length: Int): Unit = {
        storeResult(f.visitString(java.nio.CharBuffer.wrap(buffer, 0, length), cursorInt))
      }
      def onArrayStart(): Unit = {
        val ctx = f.visitArray(-1, cursorInt).narrow
        fStack = ctx.subVisitor.asInstanceOf[Visitor[_, T]] :: fStack
        visitorStack = ctx :: visitorStack
      }
      def onMapStart(): Unit = {
        val ctx = f.visitObject(-1, cursorInt).narrow
        fStack = ctx.visitKey(cursorInt).asInstanceOf[Visitor[_, T]] :: fStack
        isKey = true
        visitorStack = ctx :: visitorStack
      }
      def onBreak(): Unit = {
        isKey = false
        val end = visitorStack.head.visitEnd(cursorInt)
        visitorStack = visitorStack.tail
        fStack = fStack.tail
        storeResult(end)
      }
      def onEndOfInput(): Unit = {}
    }

    while(parser.pull(MyReceiver) != ujson.borer.DataItem.EndOfInput)()
    lastResult
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
    transform0(parser, f, input.cursor.toInt)
  }
}
