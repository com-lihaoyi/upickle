package upickle.core
/**
 * NullFacade discards all JSON AST information.
 *
 * This is the simplest possible facade. It could be useful for
 * checking JSON for correctness (via parsing) without worrying about
 * saving the data.
 *
 * It will always return () on any successful parse, no matter the
 * content.
 */
object NoOpVisitor extends Visitor[Unit, Unit] {

  def visitArray(length: Int, index: Int) = new ArrVisitor[Unit, Unit] {
    def subVisitor = NoOpVisitor.this
    def visitValue(v: Unit, index: Int): Unit = ()
    def visitEnd(index: Int): Unit = ()
  }
  def visitObject(length: Int, jsonableKeys: Boolean, index: Int) = new ObjVisitor[Unit, Unit] {
    def subVisitor = NoOpVisitor.this
    def visitKey(index: Int) = NoOpVisitor
    def visitKeyValue(s: Any): Unit = ()
    def visitValue(v: Unit, index: Int): Unit = ()
    def visitEnd(index: Int): Unit = ()
  }

  def visitNull(index: Int): Unit = ()
  def visitFalse(index: Int): Unit = ()
  def visitTrue(index: Int): Unit = ()
  def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): Unit = ()
  def visitString(s: CharSequence, index: Int): Unit = ()

  def visitFloat64(d: Double, index: Int) = ()

  def visitFloat32(d: Float, index: Int) = ()

  def visitInt8(i: Byte, index: Int) = ()
  def visitUInt8(i: Byte, index: Int) = ()

  def visitInt16(i: Short, index: Int) = ()
  def visitUInt16(i: Short, index: Int) = ()

  def visitInt32(i: Int, index: Int) = ()
  def visitUInt32(i: Int, index: Int) = ()

  def visitInt64(i: Long, index: Int) = ()
  def visitUInt64(i: Long, index: Int) = ()

  def visitFloat64String(s: String, index: Int) = ()

  def visitBinary(bytes: Array[Byte], offset: Int, len: Int, index: Int) = ()

  def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int) = ()

  def visitExt(tag: Byte, bytes: Array[Byte], offset: Int, len: Int, index: Int) = ()

  def visitChar(s: Char, index: Int) = ()
}
