package upickle.core.compat

import upickle.core.compat.Factory
import upickle.core.LinkedHashMap

import scala.collection.mutable

trait LinkedHashMapCompat[K, V]
object LinkedHashMapCompat {
  def factory[K, V]: Factory[(K, V), LinkedHashMap[K, V]] =
    new Factory[(K, V), LinkedHashMap[K, V]] {
      def fromSpecific(it: IterableOnce[(K, V)]): LinkedHashMap[K, V] =
        LinkedHashMap(it)

      def newBuilder: mutable.Builder[(K, V), LinkedHashMap[K, V]] =
        LinkedHashMap()
    }
}
