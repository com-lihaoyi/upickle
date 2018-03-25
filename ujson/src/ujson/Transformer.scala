package ujson


trait Transformer[I] {
  def transform[T](j: I, f: ujson.Visitor[_, T]): T
  def transformable[T](j: I) = Transformable.fromTransformer(j, this)
}
