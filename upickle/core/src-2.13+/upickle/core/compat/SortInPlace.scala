package upickle.core.compat

object SortInPlace {
  def apply[T, B: scala.Ordering](t: collection.mutable.ArrayBuffer[T])(f: PartialFunction[T, B]): Unit = {
    t.sortInPlaceBy(f)
  }
}

object DistinctBy{
  def apply[T, V](items: collection.Seq[T])(f: T => V) = {
    items.distinctBy(f)
  }
}