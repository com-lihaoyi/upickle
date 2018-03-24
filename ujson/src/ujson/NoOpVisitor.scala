package ujson

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

  def visitArray(index: Int) = new ArrVisitor[Unit, Unit] {
    def subVisitor = NoOpVisitor.this
    def visitValue(v: Unit, index: Int): Unit = ()
    def visitEnd(index: Int): Unit = ()
  }
  def visitObject(index: Int) = new ObjVisitor[Unit, Unit] {
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
}
