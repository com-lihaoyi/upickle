package pprint

/**
  * Summoning an implicit `TPrint[T]` provides a pretty-printed
  * string representation of the type `T`, much better than is
  * provided by the default `Type#toString`. In particular
  *
  * - More forms are properly supported and printed
  * - Prefixed Types are printed un-qualified, according to
  *   what's currently in scope
  */
trait TPrint[T]{
  def render(implicit cfg: TPrintColors): String
}

object TPrint extends TPrintGen[TPrint, TPrintColors] with TPrintLowPri{
  def literal[T](s: String) = new TPrint[T]{
    def render(implicit cfg: TPrintColors) = cfg.typeColor(s).toString
  }
  def lambda[T](f: TPrintColors => String) = new TPrint[T]{
    def render(implicit cfg: TPrintColors) = f(cfg)
  }
  def make[T](f: TPrintColors => String) = TPrint.lambda[T](f)
  def get[T](cfg: TPrintColors)(implicit t: TPrint[T]) = t.render(cfg)
  def implicitly[T](implicit t: TPrint[T]): TPrint[T] = t
  implicit val NothingTPrint: TPrint[Nothing] = TPrint.literal("Nothing")
}

case class TPrintColors(typeColor: fansi.Attrs)

object TPrintColors{
  implicit object BlackWhite extends TPrintColors(fansi.Attrs())
  object Colors extends TPrintColors(fansi.Color.Green){
    implicit val Colored = this
  }
}