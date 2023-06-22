package upickle.core

object StringVisitor extends SimpleVisitor[Nothing, Any] {
  def expectedMsg = "expected string"
  override def visitString(s: CharSequence, index: Int) = s.toString
  override def visitInt32(d: Int, index: Int) = d.toString
  override def visitInt64(d: Long, index: Int) = d.toString
  override def visitUInt64(d: Long, index: Int) = d.toString
  override def visitFloat32(d: Float, index: Int) = {
    val i = d.toInt
    if (d == i) i.toString else d.toString
  }
  override def visitFloat64(d: Double, index: Int) = {
    val i = d.toInt
    if (d == i) i.toString else d.toString
  }
  override def visitTrue(index: Int) = "true"
  override def visitFalse(index: Int) = "false"
  override def visitChar(s: Char, index: Int) = s.toString
}