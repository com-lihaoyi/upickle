package upickle.core.internal

import upickle.core.compat.Factory

object LinkedHashMapCompat {
  def factory[K, V]: Factory[(K, V), LinkedHashMap[K, V]] =
    new Factory[(K, V), LinkedHashMap[K, V]] {
      def apply() = LinkedHashMap()
      def apply(it: Nothing) = ???
    }
}
