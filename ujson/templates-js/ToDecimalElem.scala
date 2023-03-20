package ujson

object MathUtilsElem{
  def toString(bytes: Array[Elem], index: Int, s: String) = {
    val len = s.length
    var i = 0
    while (i < len) {
      bytes(index + i) = s.charAt(i).toElem
      i += 1
    }
    len
  }
}
object DoubleToDecimalElem{
  def toString(bytes: Array[Elem], index: Int, v: Double) = {
    MathUtilsElem.toString(bytes, index, v.toString)

  }
}
object FloatToDecimalElem{
  def toString(bytes: Array[Elem], index: Int, v: Float) = {
    MathUtilsElem.toString(bytes, index, v.toString)
  }
}