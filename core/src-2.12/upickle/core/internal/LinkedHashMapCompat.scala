package upickle.core.internal

import upickle.core.compat.Factory

import scala.collection.mutable

trait LinkedHashMapCompat[K, V] {
  this: LinkedHashMap[K, V] =>
  override def -=(key: K): this.type = subtractOne(key)
  override def +=(kv: (K, V)): this.type = addOne(kv)
}
object LinkedHashMapCompat {
  def factory[K, V]: Factory[(K, V), mutable.Map[K, V]] =
    new Factory[(K, V), mutable.Map[K, V]] {
      def apply() = LinkedHashMap()
      def apply(it: Nothing) = ???
    }
}
