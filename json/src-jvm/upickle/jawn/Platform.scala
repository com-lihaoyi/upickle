package upickle.json
object Platform{
  @inline def charAt(s: CharSequence, i: Int) = s.charAt(i)
  @inline def charAt(s: String, i: Int) = s.charAt(i)

}