package object pprint {
  import acyclic.pkg
  def pprintln[T: PPrint](implicit cfg: Config) = (t: T) => {
    tokenize(t)(implicitly[PPrint[T]], cfg).foreach(print)
    println()
  }
  def pprintln[T: PPrint](t: T,
                          width: Integer = null,
                          height: Integer = null,
                          indent: Integer = null,
                          colors: Colors = null)
                         (implicit cfg: Config = Config.Defaults.PPrintConfig) = {


    tokenize(t, width, height, indent, colors)(implicitly[PPrint[T]], cfg).foreach(print)
    println()
  }
  /**
   * Prettyprint a strongly-typed value, falling back to toString
   * if you don't know what to do with it. Generally used for human-facing
   * output
   */
  def tokenize[T: PPrint](t: T,
                          width: Integer = null,
                          height: Integer = null,
                          indent: Integer = null,
                          colors: Colors = null)
                         (implicit cfg: Config): Iterator[String] = {
    def f[T](t: T, t2: T): T = {
      Option(t).getOrElse(t2)
    }
    val newCfg = cfg.copy(
      width = f[Integer](width, cfg.width),
      height = f[Integer](height, cfg.height),
      indent = f[Integer](indent, cfg.indent),
      colors = f[Colors](colors, cfg.colors)
    )
    implicitly[PPrint[T]].render(t, newCfg)
  }
}

