package upickle.jawn

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
object NullFacade extends Visitor[Unit, Unit] {



  def singleContext(index: Int) = new ArrVisitor[Unit, Unit] {
    def subVisitor = NullFacade.this
    def add(v: Unit, index: Int): Unit = ()
    def finish(index: Int): Unit = ()
  }
  def arrayContext(index: Int) = new ArrVisitor[Unit, Unit] {
    def subVisitor = NullFacade.this
    def add(v: Unit, index: Int): Unit = ()
    def finish(index: Int): Unit = ()
  }
  def objectContext(index: Int) = new ObjVisitor[Unit, Unit] {
    def subVisitor = NullFacade.this
    def visitKey(s: CharSequence, index: Int): Unit = ()
    def add(v: Unit, index: Int): Unit = ()
    def finish(index: Int): Unit = ()
  }

  def jnull(index: Int): Unit = ()
  def jfalse(index: Int): Unit = ()
  def jtrue(index: Int): Unit = ()
  def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): Unit = ()
  def jstring(s: CharSequence, index: Int): Unit = ()
}
