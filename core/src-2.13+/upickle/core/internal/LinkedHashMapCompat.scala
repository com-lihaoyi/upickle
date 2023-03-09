package upickle.core.internal

import upickle.core.compat.Factory

import scala.collection.mutable

trait LinkedHashMapCompat[K, V]
object LinkedHashMapCompat {
  def factory[K, V]: Factory[(K, V), mutable.Map[K, V]] =
    new Factory[(K, V), mutable.Map[K, V]] {
      def fromSpecific(it: IterableOnce[(K, V)]): mutable.Map[K, V] =
        LinkedHashMap(it)

      def newBuilder: mutable.Builder[(K, V), mutable.Map[K, V]] =
        LinkedHashMap()
    }
}
