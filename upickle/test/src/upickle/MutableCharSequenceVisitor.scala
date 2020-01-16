package upickle

import java.nio.CharBuffer

import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

/**
  * For testing that the delegate visitor doesn't make assumptions
  * about the immutability of the CharSequences it receives.
  */
class MutableCharSequenceVisitor[T, J](v: Visitor[T, J]) extends Visitor.Delegate[T, J](v) {

  private def withMutableString[R](s: CharSequence)(f: CharSequence => R): R = {
    val buf = s.toString.toCharArray
    val r = f(CharBuffer.wrap(buf))
    // Clear the buffer with something recognizable.
    for (i <- 0 until buf.length) {
      buf(i) = 'M' // for Mutable
    }
    r
  }

  override def visitArray(length: Int, index: Int): ArrVisitor[T, J] = {
    val arr = super.visitArray(length, index)
    new ArrVisitor[T, J] {
      override def subVisitor: Visitor[_, _] = new MutableCharSequenceVisitor(arr.subVisitor)

      override def visitValue(v: T, index: Int): Unit = arr.visitValue(v, index)

      override def visitEnd(index: Int): J = arr.visitEnd(index)
    }
  }

  override def visitObject(length: Int, index: Int): ObjVisitor[T, J] = {
    val obj = super.visitObject(length, index)
    new ObjVisitor[T, J] {
      override def visitKey(index: Int): Visitor[_, _] = obj.visitKey(index)

      override def visitKeyValue(v: Any): Unit = obj.visitKeyValue(v)

      override def subVisitor: Visitor[_, _] = new MutableCharSequenceVisitor(obj.subVisitor)

      override def visitValue(v: T, index: Int): Unit = obj.visitValue(v, index)

      override def visitEnd(index: Int): J = obj.visitEnd(index)
    }
  }

  override def visitString(s: CharSequence, index: Int): J = withMutableString(s) { mut =>
    super.visitString(mut, index)
  }

  override def visitFloat64StringParts(
    s: CharSequence,
    decIndex: Int,
    expIndex: Int,
    index: Int
  ): J = withMutableString(s) { mut =>
    super.visitFloat64StringParts(mut, decIndex, expIndex, index)
  }
}
