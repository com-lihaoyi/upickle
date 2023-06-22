package upickle.core.compat

import upickle.core.LinkedHashMap

import scala.collection.mutable

trait LinkedHashMapCompat[K, V] {
  this: LinkedHashMap[K, V] =>
  override def -=(key: K): this.type = subtractOne(key)
  override def +=(kv: (K, V)): this.type = addOne(kv)
}
object LinkedHashMapCompat {
  def factory[K, V]: Factory[(K, V), LinkedHashMap[K, V]] =
    new Factory[(K, V), LinkedHashMap[K, V]] {
      def apply() = new mutable.Builder[(K, V), LinkedHashMap[K, V]] {
        private val map = LinkedHashMap[K, V]()

        def +=(elem: (K, V)): this.type = {
          map.addOne(elem)
          this
        }
        def clear(): Unit = map.clear()
        def result(): LinkedHashMap[K, V] = map
      }
      def apply(it: Nothing) = ???
    }
}
