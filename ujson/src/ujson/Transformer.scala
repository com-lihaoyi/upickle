package ujson
import upickle.core.Visitor

trait Transformer[I] extends upickle.core.Transformer[I]{
  def transformable[T](j: I) = Readable.fromTransformer(j, this)
}
