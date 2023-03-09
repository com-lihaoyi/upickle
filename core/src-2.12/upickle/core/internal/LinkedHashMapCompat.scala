package upickle.core.internal

import upickle.core.compat.Factory

trait LinkedHashMapCompat[K, V] {
  this: LinkedHashMap[K, V] =>
  override def -=(key: K): this.type = subtractOne(key)
  override def +=(kv: (K, V)): this.type = addOne(kv)
}
object LinkedHashMapCompat {
  def factory[K, V]: Factory[(K, V), LinkedHashMap[K, V]] =
    new Factory[(K, V), LinkedHashMap[K, V]] {
      def apply() = LinkedHashMap()
      def apply(it: Nothing) = ???
    }
}
