package upickle.jawn

abstract class Walker[I] {
  def visit[T](j: I, f: upickle.jawn.Visitor[_, T]): T
}
