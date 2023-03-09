package upickle.core.internal

import upickle.core.compat.Factory

import scala.collection.mutable
import java.{util => ju}

class LinkedHashMap[K, V] private (underlying: ju.LinkedHashMap[K, V])
    extends mutable.Map[K, V]
    with mutable.Builder[(K, V), LinkedHashMap[K, V]] {
  def addOne(elem: (K, V)): this.type = {
    if (elem._1 == null)
      throw new NullPointerException("null keys are not allowed")
    underlying.put(elem._1, elem._2)
    this
  }
  def iterator: Iterator[(K, V)] = {
    val it = underlying.keySet().iterator()
    new Iterator[(K, V)] {
      def hasNext: Boolean = it.hasNext()
      def next(): (K, V) = {
        val key = it.next()
        key -> underlying.get(key)
      }
    }
  }
  def get(key: K): Option[V] = Option(underlying.get(key))
  def subtractOne(elem: K): this.type = {
    underlying.remove(elem)
    this
  }
  override def put(key: K, value: V): Option[V] = {
    Option(underlying.put(key, value))
  }
  override def result(): LinkedHashMap[K, V] = this
}
object LinkedHashMap {
  def apply[K, V](): LinkedHashMap[K, V] =
    new LinkedHashMap[K, V](new ju.LinkedHashMap[K, V])
  def apply[K, V](items: TraversableOnce[(K, V)]): LinkedHashMap[K, V] = {
    val underlying = new ju.LinkedHashMap[K, V]()
    for ((key, value) <- items) {
      underlying.put(key, value)
    }
    new LinkedHashMap[K, V](underlying)
  }
  implicit def factory[K, V] = LinkedHashMapCompat.factory[K, V]
}
