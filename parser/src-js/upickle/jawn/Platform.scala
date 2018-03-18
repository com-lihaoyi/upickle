package upickle.jawn
object Platform{
  def charAt(s: CharSequence, i: Int) = {
    if (i >= s.length) throw new StringIndexOutOfBoundsException(i)
    s.charAt(i)
  }
}