package ujson
import upickle.core.Visitor

trait Transformer[I] {
  def transform[T](j: I, f: Visitor[_, T]): T
  def transformable[T](j: I) = Transformable.fromTransformer(j, this)
}
