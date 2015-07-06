package object pprint {
  import acyclic.pkg
  def pprintln[T: PPrint](implicit cfg: Config) = (t: T) => {
    PPrint.tokenize(t)(implicitly[PPrint[T]], cfg).foreach(print)
    println()
  }
  def pprintln[T: PPrint](t: T)(implicit cfg: Config) = {
    PPrint.tokenize(t)(implicitly[PPrint[T]], cfg).foreach(print)
    println()
  }
}
