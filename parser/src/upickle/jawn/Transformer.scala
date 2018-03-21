package upickle.jawn

trait Transformer[I] {
  def transform[T](j: I, f: upickle.jawn.Visitor[_, T]): T
}
