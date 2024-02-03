package upickle.core.compat

object SortInPlace {
  def apply[T, B](t: collection.mutable.ArrayBuffer[T])(f: T => B): Unit = {
    val sorted = t.sortBy(f)
    t.clear()
    t.appendAll(sorted)
  }
}
