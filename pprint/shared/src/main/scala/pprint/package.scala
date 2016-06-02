package object pprint {
  import acyclic.pkg

  /**
    * Pretty-prints something to the console for you to look at, with
    * a bunch of additional metadata (function-name, line-num, optional tag, etc.)
    * to make debugging easier
    */
  def log[T: PPrint](value: sourcecode.Text[T], tag: String = "", verbose: Boolean = false)
                     (implicit cfg: Config = Config.Colors.PPrintConfig,
                      path: sourcecode.Name,
                      line: sourcecode.Line) = {
    val titleIter = path.value.toString
    val tagIter =
      if (tag == "") ""
      else " " + tokenize(tag).mkString
    val lineIter = ":" + tokenize(line.value).mkString

    val valName = value.source
    val item = tokenize(value.value).mkString

    println(
      cfg.colors.prefixColor(titleIter + tagIter).render +
      lineIter +
      " " +
      valName +
      "\t" +
      item
    )
  }

  /**
    * More verbose version of [[log]]
    */
  def log2[T: PPrint](value: sourcecode.Text[T], tag: String = "", verbose: Boolean = false)
                    (implicit cfg: Config = Config.Colors.PPrintConfig,
                     path: sourcecode.Enclosing,
                     line: sourcecode.Line) = {
    val titleIter = path.value.toString
    val tagIter =
      if (tag == "") ""
      else " " + tokenize(tag).mkString
    val lineIter = ":" + tokenize(line.value).mkString

    println(cfg.colors.prefixColor(titleIter + tagIter).render + lineIter)
    pprintln(value.value)


  }
  def pprintln[T: PPrint](implicit cfg: Config) = (t: T) => {
    tokenize(t)(implicitly[PPrint[T]], cfg).foreach(print)
    println()
  }
  /**
    * Pretty-prints something to the console for you to look at.
    */
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
                         (implicit cfg: Config = Config.Defaults.PPrintConfig): Iterator[String] = {
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
  def tprint[T: TPrint](implicit config: TPrintColors) = {
    implicitly[TPrint[T]].render(config)
  }
}

