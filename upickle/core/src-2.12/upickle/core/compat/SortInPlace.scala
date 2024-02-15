package upickle.core.compat

object SortInPlace {
  def apply[T, B: Ordering](t: collection.mutable.ArrayBuffer[T])(f: PartialFunction[T, B]): Unit = {
    val sorted = t.sortBy(f)
    t.clear()
    t.appendAll(sorted)
  }
}

object DistinctBy{
  def apply[T, V](items: collection.Seq[T])(f: T => V) = {
    val output = collection.mutable.Buffer.empty[T]
    val seen = collection.mutable.Set.empty[V]
    for(item <- items){
      val key = f(item)
      if (!seen(key)) {
        seen.add(key)
        output.append(item)
      }
    }
    output
  }
}