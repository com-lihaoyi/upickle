package upickle.core

/**
  * A visitor that throws an error for all the visit methods which it does not define,
  * letting you only define the handlers you care about.
  */
class LogVisitor[-T, +V](downstream: Visitor[T, V], log: String => Unit = println, indent: String = "    ") extends Visitor[T, V] {
  def visitNull(index: Int): V = {
    log(s"visitNull($index)")
    downstream.visitNull(index)
  }
  def visitTrue(index: Int): V = {
    log(s"visitTrue($index)")
    downstream.visitTrue(index)
  }
  def visitFalse(index: Int): V = {
    log(s"visitFalse($index)")
    downstream.visitFalse(index)
  }

  def visitString(s: CharSequence, index: Int): V = {
    log(s"visitString($s, $index)")
    downstream.visitString(s, index)
  }
  def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): V = {
    log(s"visitFloat64StringParts($s, $decIndex, $expIndex, $index)")
    downstream.visitFloat64StringParts(s, decIndex, expIndex, index)
  }

  def visitObject(length: Int, index: Int): ObjVisitor[T, V] = {
    log(s"visitObject($length, $index)")

    val d = downstream.visitObject(length, index)
    new ObjVisitor[T, V] {
      def logIndented(s: String) = log(indent + s)
      def visitKey(index: Int): Visitor[_, _] = {
        logIndented(s"visitKey($index)")
        new LogVisitor(d.visitKey(index), s => logIndented(indent + s))
      }
      def visitKeyValue(v: Any): Unit = {
        logIndented(s"visitKeyValue($v)")
        d.visitKeyValue(v)
      }

      def subVisitor: Visitor[_, _] = {
        logIndented(s"subVisitor")
        new LogVisitor(d.subVisitor, s => logIndented(indent + s))
      }
      def visitValue(v: T, index: Int): Unit = {
        logIndented(s"visitValue($v, $index)")
        d.visitValue(v, index)
      }
      def visitEnd(index: Int) = {
        logIndented(s"visitEnd($index)")
        d.visitEnd(index)
      }
    }
  }
  def visitArray(length: Int, index: Int): ArrVisitor[T, V] = {
    log(s"visitArray($length, $index)")
    val d = downstream.visitArray(length, index)
    new ArrVisitor[T, V] {
      def logIndented(s: String) = log("  " + s)
      def subVisitor: Visitor[_, _] = {
        logIndented(s"subVisitor")
        new LogVisitor(d.subVisitor, s => logIndented("  " + s))
      }
      def visitValue(v: T, index: Int): Unit = {
        logIndented(s"visitValue($v, $index)")
        d.visitValue(v, index)
      }
      def visitEnd(index: Int) = {
        logIndented(s"visitEnd($index)")
        d.visitEnd(index)
      }
    }
  }

  def visitFloat64(d: Double, index: Int): V = {
    log(s"visitFloat64($d, $index)")
    downstream.visitFloat64(d, index)
  }

  def visitFloat32(d: Float, index: Int): V = {
    log(s"visitFloat32($d, $index)")
    downstream.visitFloat32(d, index)
  }

  def visitInt32(i: Int, index: Int): V = {
    log(s"visitInt32($i, $index)")
    downstream.visitInt32(i, index)
  }

  def visitInt64(i: Long, index: Int): V = {
    log(s"visitInt64($i, $index)")
    downstream.visitInt64(i, index)
  }

  def visitUInt64(i: Long, index: Int): V = {
    log(s"visitUInt64($i, $index)")
    downstream.visitUInt64(i, index)
  }

  def visitFloat64String(s: String, index: Int): V = {
    log(s"visitFloat64String($s, $index)")
    downstream.visitFloat64String(s, index)
  }

  def visitChar(s: Char, index: Int): V = {
    log(s"visitChar($s, $index)")
    downstream.visitChar(s, index)
  }

  def visitBinary(bytes: Array[Byte], offset: Int, len: Int, index: Int): V = {
    log(s"visitBinary($bytes, $offset, $len, $index)")
    downstream.visitBinary(bytes, offset, len, index)
  }

  def visitExt(tag: Byte, bytes: Array[Byte], offset: Int, len: Int, index: Int): V = {
    log(s"visitExt($tag, $bytes, $offset, $len, $index)")
    downstream.visitExt(tag, bytes, offset, len, index)
  }
}
