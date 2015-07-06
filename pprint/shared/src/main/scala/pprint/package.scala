package object pprint {
  import acyclic.pkg
  def pprintln[T: PPrint] = (t: T) => {
    PPrint(t)(implicitly[PPrint[T]]).foreach(print)
    println()
  }
  def pprintln[T: PPrint](t: T) = {
    PPrint(t)(implicitly[PPrint[T]]).foreach(print)
    println()
  }
}
