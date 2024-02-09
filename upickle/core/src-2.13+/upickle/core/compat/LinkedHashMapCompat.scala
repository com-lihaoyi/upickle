package upickle.core.compat

import upickle.core.LinkedHashMap
import upickle.core.compat.Factory

import scala.collection.mutable

trait LinkedHashMapCompat[K, V]
object LinkedHashMapCompat {
  def factory[K, V]: Factory[(K, V), LinkedHashMap[K, V]] =
    new Factory[(K, V), LinkedHashMap[K, V]] {
      def fromSpecific(it: IterableOnce[(K, V)]): LinkedHashMap[K, V] =
        LinkedHashMap(it)

      def newBuilder: mutable.Builder[(K, V), LinkedHashMap[K, V]] =
        new mutable.Builder[(K, V), LinkedHashMap[K, V]] {
          private val map = LinkedHashMap[K, V]()

          def addOne(elem: (K, V)): this.type = {
            map.addOne(elem)
            this
          }
          def clear(): Unit = map.clear()
          def result(): LinkedHashMap[K, V] = map
        }
    }
}
