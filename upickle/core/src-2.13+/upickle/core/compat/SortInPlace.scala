package upickle.core.compat

object SortInPlace {
  def apply[T, B: scala.Ordering](t: collection.mutable.ArrayBuffer[T])(f: T => B): Unit = {
    t.sortInPlaceBy(f)
  }
}
