package pprint
import language.higherKinds
import acyclic.file

/**
 * Configuration options to control how prettyprinting occurs, passed
 * recursively into each prettyprinting callsite.
 *
 * @param width Controls how far to the right a line will go before
 *                 it tries to wrap
 * @param height Controls how many lines can be printed at once.
 *                  Will print all lines if set to 0
 * @param depth How much the current item being printed should be indented
 * @param renames A map used to rename things to more common names, e.g.
 *                renamig `WrappedArray` to `Array` or getting rid of
 *                TupleN *
 * @param showFieldName Should the field name of a case class be displayed
                        alongside its value, e.g. `Foo(a = 3)`.
 */
case class Config(width: Int = Config.defaultMaxWidth,
                  height: Int = Config.defaultLines,
                  depth: Int = 0,
                  indent: Int = Config.defaultIndent,
                  colors: Colors = pprint.Colors.BlackWhite,
                  renames: Map[String, String] = Config.defaultRenames,
                  showFieldName: Boolean = false)
  extends GenConfig[Config]{
  def deeper = copy(depth = depth + 1)

  def rename(s: String) = renames.getOrElse(s, s)

}
case class Colors(literalColor: fansi.Attrs,
                  prefixColor: fansi.Attrs)
object Colors{
  val BlackWhite = Colors(fansi.Attrs(), fansi.Attrs())
  val Colored = Colors(fansi.Color.Green, fansi.Color.Yellow)
}

object Config {
  val defaultMaxWidth = 100
  val defaultLines = 0
  val defaultIndent = 2
  val defaultRenames = Map(
    "WrappedArray" -> "Array"
  ) ++ (2 to 22).map(i => s"Tuple$i" -> "")

  object Defaults {
    implicit val PPrintConfig = Config()
  }
  object Colors {
    implicit val PPrintConfig = Config(
      colors = pprint.Colors.Colored
    )
  }
}

/**
 * Helpers to help inject behavior into the generated code
 * without having any circular dependencies
 */
trait GenConfig[T <: GenConfig[T]]{
  def deeper: T
  def rename(s: String): String
}

/**
 * Helpers to help inject behavior into the generated code
 * without having any circular dependencies
 */
trait GenUtils{
  type PP[T]
  type C <: GenConfig[C]
  def render[T: PP](t: T, c: C): Iterator[String]
  type Chunker[T]
  def makeChunker[T](f: (T, C) => Iterator[Iterator[String]]): Chunker[T]
}
