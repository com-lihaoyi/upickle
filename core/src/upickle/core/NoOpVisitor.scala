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
  def visitObject(length: Int, index: Int) = new ObjVisitor[Unit, Unit] {
    def subVisitor = NoOpVisitor.this
    def visitKey(s: CharSequence, index: Int): Unit = ()
    def visitValue(v: Unit, index: Int): Unit = ()
    def visitEnd(index: Int): Unit = ()
  }

  def visitNull(index: Int): Unit = ()
  def visitFalse(index: Int): Unit = ()
  def visitTrue(index: Int): Unit = ()
  def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): Unit = ()
  def visitString(s: CharSequence, index: Int): Unit = ()

  def visitNumRaw(d: Double, index: Int) = ()

  def visitNum32(d: Float, index: Int) = ()

  def visitInt32(i: Int, index: Int) = ()
  def visitUInt32(i: Int, index: Int) = ()

  def visitInt64(i: Long, index: Int) = ()
  def visitUInt64(i: Long, index: Int) = ()

  def visitNumRawString(s: String, index: Int) = ()

  def visitBin(bytes: Array[Byte], offset: Int, len: Int, index: Int) = ()

  def visitNum(s: CharSequence, decIndex: Int, expIndex: Int) = ()
}
