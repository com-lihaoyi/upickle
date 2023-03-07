package upickle.core.internal

import upickle.core.compat.Factory

import scala.collection.mutable

object LinkedHashMapCompat {
  def factory[K, V]: Factory[(K, V), LinkedHashMap[K, V]] =
    new Factory[(K, V), LinkedHashMap[K, V]] {
      def fromSpecific(it: IterableOnce[(K, V)]): LinkedHashMap[K, V] =
        LinkedHashMap(it)

      def newBuilder: mutable.Builder[(K, V), LinkedHashMap[K, V]] =
        LinkedHashMap()
    }
}
