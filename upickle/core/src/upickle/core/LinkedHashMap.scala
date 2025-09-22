package upickle.core

import upickle.core.compat._

import scala.collection.mutable
import java.{util => ju}

/** mutable.Map[K, V] implementation wrapping a java.util.LinkedHashMap[K, V]
  * which doesn't allow null as key. Useful since the java.util implementation
  * is safe from hash-collision attacks.
  */
class LinkedHashMap[K, V] private (underlying: ju.LinkedHashMap[K, V])
    extends mutable.Map[K, V]
    with LinkedHashMapCompat[K, V] {
  override def size: Int = underlying.size()
  override def clear(): Unit = underlying.clear()
  override def isEmpty: Boolean = underlying.isEmpty()
  private def _put(key: K, value: V): V = {
    if (key == null)
      throw new NullPointerException("null keys are not allowed")
    underlying.put(key, value)
  }
  def addOne(elem: (K, V)): this.type = {
    _put(elem._1, elem._2)
    this
  }
  def iterator: Iterator[(K, V)] = {
    val it = underlying.entrySet().iterator()
    new Iterator[(K, V)] {
      def hasNext: Boolean = it.hasNext()
      def next(): (K, V) = {
        val e = it.next()
        (e.getKey, e.getValue)
      }
    }
  }
  def foreach[U](f: (K, V) => U): Unit = underlying.forEach(new ju.function.BiConsumer[K, V] {
    override def accept(t: K, u: V): Unit = f(t, u)
  })
  def get(key: K): Option[V] = Option(underlying.get(key))
  def subtractOne(elem: K): this.type = {
    underlying.remove(elem)
    this
  }
  override def put(key: K, value: V): Option[V] = {
    Option(_put(key, value))
  }
  override def result(): LinkedHashMap[K, V] = this
}
object LinkedHashMap {

  def apply[K, V](): LinkedHashMap[K, V] =
    new LinkedHashMap[K, V](new ju.LinkedHashMap[K, V])

  def apply[K, V](items: IterableOnce[(K, V)]): LinkedHashMap[K, V] = {
    val map = LinkedHashMap[K, V]()
    toIterator(items).foreach { case (key, value) =>
      map._put(key, value)
    }
    map
  }
  implicit def factory[K, V]: Factory[(K, V), LinkedHashMap[K, V]] =
    LinkedHashMapCompat.factory[K, V]
}
