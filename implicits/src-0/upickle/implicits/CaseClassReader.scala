package upickle.implicits

import upickle.core.{ Visitor, ObjVisitor }

trait CaseClassReaderPiece:
  this: upickle.core.Types with Readers =>
  trait CaseClassReader[T] extends SimpleReader[T]:
    def make(bldr: Map[String, Any]): T

    def visitorForKey(currentKey: String): Visitor[_, _]

    override def expectedMsg = "expected case class"

    override def visitObject(length: Int, index: Int) = new ObjVisitor[Any, T] {
      var currentKey: String = null
      private val builder = collection.mutable.Map.empty[String, Any]

      def subVisitor: Visitor[_, _] = visitorForKey(currentKey)

      def visitKey(index: Int): Visitor[_, _] = StringReader

      def visitKeyValue(v: Any): Unit =
        currentKey = v.asInstanceOf[String]

      def visitValue(v: Any, index: Int): Unit =
        builder(currentKey) = v

      def visitEnd(index: Int): T =
        make(builder.toMap)
    }
  end CaseClassReader

  // For each field:
  // 1. Get its name and type T
  // 2. Resolve Reader[T]
  // 3. Resolve its default parameter if exists
  def mkReader[T](visitors: Map[String, Visitor[_, _]],
      maker: Map[String, Any] => T): Reader[T] = new CaseClassReader[T] {
    override def visitorForKey(key: String) = visitors(key)
    override def make(params: Map[String, Any]): T = maker(params)
  }
end CaseClassReaderPiece
