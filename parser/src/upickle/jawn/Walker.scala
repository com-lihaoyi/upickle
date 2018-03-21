package upickle.jawn

trait Walker[I] {
  def walk[T](j: I, f: upickle.jawn.Visitor[_, T]): T
}
