package upickle.jawn

abstract class Visitor[I] {
  def visit[T](j: I, f: upickle.jawn.RawFacade[_, T]): T
}
