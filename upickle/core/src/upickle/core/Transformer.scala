package upickle.core

trait Transformer[I] {
  def transform[T](j: I, f: Visitor[_, T]): T
}
