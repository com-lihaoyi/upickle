package upickle.core

class WrapElemArrayCharSeq(arr: Array[Elem], start: Int, length: Int) extends CharSequence{
  assert(
    start < arr.length && start + length <= arr.length && start >= 0 && length >= 0,
    s"WrapElemArrayCharSeq(${arr.length}, $start, $length)"
  )
  def length() = WrapElemArrayCharSeq.this.length

  def charAt(index: Int) = {
    arr(index + start).toChar
  }

  def subSequence(newStart: Int, newEnd: Int) = {
    new WrapElemArrayCharSeq(arr, start + newStart, newEnd - newStart)
  }

  private[this] var toString0: String = null
  override def toString = {
    if (toString0 != null) toString0
    else {
      val res = new String(arr, start, length)
      toString0 = res
      res
    }
  }
}
