package upickle.core

object StringVisitor extends CustomVisitor[Nothing, Any] {
  def expectedMsg = "expected string"
  override def visitString(s: CharSequence, index: Int) = s
}