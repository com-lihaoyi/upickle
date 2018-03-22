package upickle.json

trait Transformer[I] {
  def transform[T](j: I, f: upickle.json.Visitor[_, T]): T
}
