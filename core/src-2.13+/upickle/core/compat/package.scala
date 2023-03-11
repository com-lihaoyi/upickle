package upickle.core

package object compat {

  type Factory[-A, +C] = collection.Factory[A, C]

  def toIterator[T](iterable: IterableOnce[T]) = iterable.iterator

}
