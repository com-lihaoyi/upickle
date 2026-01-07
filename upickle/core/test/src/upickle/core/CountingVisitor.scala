package upickle.core

/**
  * A counting visitor that tracks how many elements were visited
  * and what indices were passed. Used to verify the parser handles
  * large files without overflow.
  */
class CountingVisitor extends Visitor[Unit, Unit] {
  var elementCount: Long = 0L
  var negativeIndexCount: Long = 0L
  var maxIndex: Int = Int.MinValue

  // Track counts by type for verification
  var nullCount: Long = 0L
  var boolCount: Long = 0L
  var numberCount: Long = 0L
  var stringCount: Long = 0L
  var arrayCount: Long = 0L
  var objectCount: Long = 0L
  var binaryCount: Long = 0L

  def visitArray(length: Int, index: Int) = {
    arrayCount += 1
    trackIndex(index)
    new CountingArrVisitor(this)
  }

  def visitObject(length: Int, jsonableKeys: Boolean, index: Int) = {
    objectCount += 1
    trackIndex(index)
    new CountingObjVisitor(this)
  }

  private def trackIndex(index: Int): Unit = {
    if (index < 0) negativeIndexCount += 1
    else if (index > maxIndex) maxIndex = index
  }

  def visitNull(index: Int): Unit = { elementCount += 1; nullCount += 1; trackIndex(index) }
  def visitFalse(index: Int): Unit = { elementCount += 1; boolCount += 1; trackIndex(index) }
  def visitTrue(index: Int): Unit = { elementCount += 1; boolCount += 1; trackIndex(index) }
  def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): Unit = {
    elementCount += 1; numberCount += 1; trackIndex(index)
  }
  def visitString(s: CharSequence, index: Int): Unit = { elementCount += 1; stringCount += 1; trackIndex(index) }
  def visitFloat64(d: Double, index: Int): Unit = { elementCount += 1; numberCount += 1; trackIndex(index) }
  def visitFloat32(d: Float, index: Int): Unit = { elementCount += 1; numberCount += 1; trackIndex(index) }
  def visitInt32(i: Int, index: Int): Unit = { elementCount += 1; numberCount += 1; trackIndex(index) }
  def visitInt64(i: Long, index: Int): Unit = { elementCount += 1; numberCount += 1; trackIndex(index) }
  def visitUInt64(i: Long, index: Int): Unit = { elementCount += 1; numberCount += 1; trackIndex(index) }
  def visitFloat64String(s: String, index: Int): Unit = { elementCount += 1; numberCount += 1; trackIndex(index) }
  def visitBinary(bytes: Array[Byte], offset: Int, len: Int, index: Int): Unit = { elementCount += 1; binaryCount += 1; trackIndex(index) }
  def visitExt(tag: Byte, bytes: Array[Byte], offset: Int, len: Int, index: Int): Unit = { elementCount += 1; trackIndex(index) }
  def visitChar(s: Char, index: Int): Unit = { elementCount += 1; stringCount += 1; trackIndex(index) }
}

class CountingArrVisitor(parent: CountingVisitor) extends ArrVisitor[Unit, Unit] {
  def subVisitor = parent
  def visitValue(v: Unit, index: Int): Unit = {
    if (index < 0) parent.negativeIndexCount += 1
  }
  def visitEnd(index: Int): Unit = {
    if (index < 0) parent.negativeIndexCount += 1
  }
}

class CountingObjVisitor(parent: CountingVisitor) extends ObjVisitor[Unit, Unit] {
  def subVisitor = parent
  def visitKey(index: Int) = parent
  def visitKeyValue(s: Any): Unit = ()
  def visitValue(v: Unit, index: Int): Unit = {
    if (index < 0) parent.negativeIndexCount += 1
  }
  def visitEnd(index: Int): Unit = {
    if (index < 0) parent.negativeIndexCount += 1
  }
}
