package pprint

import derive.Derive

import scala.language.experimental.macros
import language.higherKinds
import annotation.tailrec
import acyclic.file
import scala.util.matching.Regex.Match
import scala.{Iterator => Iter}
import compat._
import language.existentials
/**
 * There are a few main classes involved here:
 *
 * - [[PPrint]]: an all-encompassing typeclass that contains
 *   all you need to pretty-print a value T => Iter[String]
 *
 * - [[PPrinter]]: a typeclass that takes a configuration
 *   object and pretty-prints a value (T, Config) => Iter[String]
 *
 * - [[Config]]: controls various metadata about the pprint, e.g. how
 *   wide to go before we start wrapping, or what colors to use
 *
 * - [[Chunker]]: (T, Config) => Iter[ Iter[String] ]. Implements the
 *   very common comma-separated-values pattern, where each sub-iter
 *   is one of the comma separated blocks, but it's up to the caller
 *   to decide what to do with them
 */

trait Chunker[T]{
  def chunk(t: T, c: Config): Iter[Iter[String]]
}
object Chunker extends PPrinterGen {
  def chunk[T: Chunker](t: T, c: Config): Iter[Iter[String]] = implicitly[Chunker[T]].chunk(t, c)
  def apply[T](f: (T, Config) => Iter[Iter[String]]) = new Chunker[T]{
    def chunk(t: T, c: Config) = f(t, c)
  }
  type UP[T] = Internals.Unpacker[T]
  type PP[T] = PPrint[T]
  type C = Config
  type Chunker[T] = pprint.Chunker[T]

  /**
   * Special, because `Product0` doesn't exist
   */

  implicit def Tuple0Chunker = Chunker((t: Unit, cfg: Config) => Iter[Iter[String]]())
//  implicit def TupleNothing1Chunker[T1: Chunker.PP]: Chunker[Tuple1[Nothing]] = Chunker.makeChunker{
//    (t: Tuple1[T1], cfg: Config) => Iterator()
//  }
  def makeChunker[T](f: (T, Config) => Iter[Iter[String]]) = Chunker(f)
  def render[T: PP](t: T, c: Config) = implicitly[PPrint[T]].pprinter.render(t, c)

}
object PPrint extends Internals.LowPriPPrint{

  def apply[A](pprinter0: PPrinter[A]) = new PPrint[A] {
    val pprinter = pprinter0
    def render(t: A, cfg: Config): Iter[String] = {
      if (t == null) Iter("null")
      else pprinter.render(t, cfg)
    }
  }


  type PPrint[A] = pprint.PPrint[A]
  object Knot {

    case class PPrint[A](f0: () => pprint.PPrint[A]) extends pprint.PPrint[A] {
      lazy val f = f0()

      def render(t: A, cfg: Config) = f.render(t, cfg)

      def pprinter = f.pprinter
    }

  }
  /**
   * Helper to make implicit resolution behave right
   */
  implicit def Contra[A](implicit ca: PPrinter[A]): PPrint[A] = PPrint(ca)

}


/**
 * A typeclass necessary to prettyprint something. Separate from [[PPrinter]]
 * in order to make contravariant implicit resolution behave right.
 */
trait PPrint[A]{
  def render(t: A, cfg: Config): Iter[String]
  def pprinter: PPrinter[A]
}

/**
 * A typeclass you define to prettyprint values of type [[A]]
 */
trait PPrinter[-A] {
  def render(t: A, c: Config): Iter[String] = {
    if (t == null) Iter("null")
    else render0(t, c)
  }
  def render0(t: A, c: Config): Iter[String]


}

object PPrinter extends LowPriPPrinter{
  // Things being injected into PPrinterGen to keep it acyclic
  
  def apply[T](r: (T, Config) => Iter[String]): PPrinter[T] = {
    new PPrinter[T]{ 
      def render0(t: T, c: Config) = {
        if(c.height > 0)
          takeFirstLines(c, r(t, c))
        else r(t, c)
      }
    }
  }

  /**
   * A basic [[PPrinter]] that does `toString` and nothing else
   */
  def Literal: PPrinter[Any] = PPrinter((t, c) => Iter(t.toString))

  /**
   * A [[PPrinter]] that does `toString`, with an optional
   * color
   */
  def literalColorPPrinter[T](map: String => String = x => x): PPrinter[T] = PPrinter[T] { (t: T, c: Config) =>
    Iter(c.colors.literalColor(map(t.toString)).render)
  }

  implicit val UnitRepr = PPrinter[Unit] { (t: Unit, c: Config) =>
    Iter(c.colors.literalColor("()").render)
  }

  implicit val NullRepr = literalColorPPrinter[Null]()
  implicit val BooleanRepr = literalColorPPrinter[Boolean]()
  implicit val ByteRepr = literalColorPPrinter[Byte]()
  implicit val ShortRepr = literalColorPPrinter[Short]()
  implicit val IntRepr = literalColorPPrinter[Int]()
  implicit val LongRepr = literalColorPPrinter[Long](_+"L")
  implicit val FloatRepr = literalColorPPrinter[Float](_+"F")
  implicit val DoubleRepr = literalColorPPrinter[Double]()
  implicit val CharRepr = literalColorPPrinter[Char] { x => "'" + escape(x.toString) + "'" }
  implicit def ChunkedRepr[T <: Product: Chunker] = PPrinter[T]{ (t, c) =>
    if (t == null) Iterator("null")
    else Internals.handleChunks(t.productPrefix, c, implicitly[Chunker[T]].chunk(t, _))
  }
  val escapeSet = "\"\n\r\t\\".toSet

  implicit val StringRepr = PPrinter[String] { (x, c) =>
    // We break up the string into chunks and only lazily
    // encode (escape or indent) it for display. This ensures
    // that extra-large strings can start streaming immediately
    // without encoding the whole string.
    //
    // We are forced to check the whole string for special escapes
    // before deciding on which way to encode/display it, but
    // that's unavoidable, doesn't allocate any memory, and
    // hopefully fast
    val chunkSize = 128

    val chunks =
      for(i <- (0 until x.length by chunkSize).iterator)
      yield x.slice(i, i + chunkSize)

    val body =
      if (!x.exists(escapeSet)) Iter("\"") ++ chunks.map(escape) ++ Iter("\"")
      else {
        val indent = "  " * c.depth
        val indented = chunks.map(_.replace("\n", indent + "\n"))
        Iter("\"\"\"\n") ++ indented ++ Iter("\n", indent, "\"\"\"")
      }

    // This is a really hacky way to extract the color-prefix and reset-suffix
    // from a fansi.Attrs value. Fansi should expose some way to do this
    // directly, but until that happens this will do
    val snippet = c.colors.literalColor(" ").render
    val prefix = fansi.Attrs.emitAnsiCodes(0, c.colors.literalColor.applyMask)
    val suffix = fansi.Attrs.emitAnsiCodes(c.colors.literalColor.applyMask, 0)

    Iter(prefix) ++ body ++ Iter(suffix)
  }
  implicit val SymbolRepr = PPrinter[Symbol]((x, c) =>
    Iter(c.colors.literalColor("'" + x.name).render)
  )

  /**
   * Escapes a string to turn it back into a string literal
   */
  def escape(text: String): String = {
    val s = new StringBuilder
    val len = text.length
    var pos = 0
    var prev = 0

    @inline
    def handle(snip: String) = {
      s.append(text.substring(prev, pos))
      s.append(snip)
    }
    while (pos < len) {
      text.charAt(pos) match {
        case '"' => handle("\\\""); prev = pos + 1
        case '\n' => handle("\\n"); prev = pos + 1
        case '\r' => handle("\\r"); prev = pos + 1
        case '\t' => handle("\\t"); prev = pos + 1
        case '\\' => handle("\\\\"); prev = pos + 1
        case _ =>
      }
      pos += 1
    }
    handle("")
    s.toString()
  }

  val ansiRegex = "\u001B\\[[;\\d]*m".r.pattern
  private def takeFirstLines(cfg: Config, iter: Iter[String]): Iter[String] = {
    // Calculates how many lines and characters are remaining after printing the given string.
    // Also returns how much of this string can be printed if the space runs out
    @tailrec
    def charIter(str: String, pos: Int, lines: Int, chars: Int): (Int, Int, Option[Int]) = {
      if(pos >= str.length) (lines, chars, None)
      else if(lines == 1 && chars == 0){
        // this would be the first character wrapping into the first line not printed
        (0, 0, Some(pos))
      } else {
        val m = ansiRegex.matcher(str)
        m.region(pos, str.length)
        if (m.lookingAt()) {
          charIter(str, pos + m.end, lines, chars)
        }else{
          val (remainingLines, remainingChars) =
            if(str(pos) == '\n') (lines - 1, cfg.width) //starting a new line
            else if(chars == 0) (lines - 1, cfg.width - 1) //wrapping around and printing a character
            else (lines, chars - 1) //simply printing a character
          if(remainingLines == 0) (lines, chars, Some(pos + 1))
          else charIter(str, pos + 1, remainingLines, remainingChars)
        }
      }
    }

    @tailrec
    def strIter(lines: Int, chars: Int, begin: Iter[String]): Iter[String] = {
      if(!iter.hasNext) begin
      else if(lines == 0) begin ++ Iter(cfg.colors.prefixColor("...").render)
      else{
        val head = iter.next
        val (remainingLines, remainingChars, substringLength) = charIter(head, 0, lines, chars)
        if(!substringLength.isEmpty){
          begin ++ Iter(
            head.substring(0, substringLength.get),
            cfg.colors.prefixColor("...").render
          )
        } else {
          strIter(remainingLines, remainingChars, begin ++ Iter(head))
        }
      }
    }
    strIter(cfg.height, cfg.width, Iter.empty)
  }

  implicit def ArrayRepr[T: PPrint] = PPrinter[Array[T]]{
    def repr = Internals.collectionRepr[T, Seq[T]]
    (t: Array[T], c: Config) => repr.render(t, c)
  }

  implicit def MapRepr[T: PPrint, V: PPrint] = Internals.makeMapRepr[collection.Map, T, V]
}

trait LowPriPPrinter{
  implicit def SeqRepr[T: PPrint, V[T] <: Traversable[T]]  =
    Internals.collectionRepr[T, V[T]]

}


object Internals {

  def makeMapRepr[M[T, V] <: collection.Map[T, V], T: PPrint, V: PPrint] = {
    PPrinter[M[T, V]] { (t: M[T, V], c: Config) =>
      handleChunks(t.stringPrefix, c, { c =>
        t.iterator.map{ case (t, v) =>
          implicitly[PPrint[T]].pprinter.render(t, c) ++
          Iter(" -> ") ++
          implicitly[PPrint[V]].pprinter.render(v, c)
        }
      })
    }
  }

  def collectionRepr[T: PPrint, V <: Traversable[T]]: PPrinter[V] = PPrinter[V] {
    (i: V, c: Config) => {
      def cFunc = (cfg: Config) => i.toIterator.map(implicitly[PPrint[T]].pprinter.render(_, cfg))

      // Streams we always print vertically, because they're lazy and
      // we don't know how long they will end up being.
      if (!i.isInstanceOf[Stream[T]]) handleChunks(i.stringPrefix, c, cFunc)
      else handleChunksVertical(i.stringPrefix, c, cFunc)
    }
  }

  /**
   * Renders something that looks like
   *
   * Prefix(inner, inner, inner)
   *
   * or
   *
   * Prefix(
   *   inner,
   *   inner,
   *   inner
   * )
   *
   * And deals with the necessary layout considerations to
   * decide whether to go vertical or horizontal
   */
  def handleChunks(name: String,
                   c: Config,
                   chunkFunc: Config => Iter[Iter[String]]): Iter[String] = {

    val renamed = c.rename(name)
    // Prefix, contents, and all the extra ", " "(" ")" characters
    val horizontalChunks =
      chunkFunc(c).flatMap(", " +: _.toStream)
                  .toStream
                  .drop(1)
    val effectiveWidth = c.width - (c.depth * c.indent)
    // Make sure we don't read more from the `chunks` stream that we
    // have to before deciding to go vertically.
    //
    // This keeps the pprinting lazy, ensuring you can pprint arbitrarily
    // collections only ever traversing approximately the amount you need
    // before truncation, and never the whole thing
    @tailrec def checkOverflow(chunks: Stream[String], currentWidth: Int): Boolean = chunks match{
      case Stream.Empty => false
      case head #:: rest =>
        if (head.contains("\n")) true
        else {
          val nextWidth = currentWidth + head.replaceAll(ansiRegex, "").length
          if (nextWidth > effectiveWidth) true
          else checkOverflow(rest, nextWidth)
        }
    }
    val overflow = checkOverflow(horizontalChunks, renamed.length + 2)

    if (overflow) handleChunksVertical(name, c, chunkFunc)
    else Iter(c.colors.prefixColor(renamed).render, "(") ++ horizontalChunks ++ Iter(")")
  }
  val ansiRegex = "\u001B\\[[;\\d]*m"

  /**
   * Same as `handleChunks`, but lays things out vertically instead of trying
   * to make a choice. Apart from being delegated to in `handleChunks`, the
   * `Stream` printer uses this directly.
   */
  def handleChunksVertical(name: String,
                           c: Config,
                           chunkFunc: Config => Iter[Iter[String]]): Iter[String] = {

    val chunks2 = chunkFunc(c.deeper)

    // Needs to be a def to avoid exhaustion
    def indent = Iter.fill(c.depth)("  ")

    Iter(c.colors.prefixColor(c.rename(name)).render, "(\n") ++
    chunks2.flatMap(Iter(",\n", "  ") ++ indent ++ _).drop(1) ++
    Iter("\n") ++ indent ++ Iter(")")
  }

  type Unpacker[T] = (T, Config) => Iter[Iter[String]]


  trait LowPriPPrint extends LowPri2{
    implicit def NullPPrint: PPrint[Null] = PPrint(PPrinter.NullRepr)
//    implicit def NothingPPrint: PPrint[Nothing] = PPrint[Nothing](PPrinter[Nothing]((t: Nothing, c: Config) => Iter()))

  }
  trait LowPri2{
    implicit def FinalRepr[T]: PPrint[T] = macro LowerPriPPrint.FinalRepr[T]
    def annotate[V](pp: PPrint[V], n: String) = pp
  }

  def fromUnpacker[T](prefix: T => String)(f: Internals.Unpacker[T]): PPrinter[T] = PPrinter[T]{
    (t: T, c: Config) => Internals.handleChunks(prefix(t), c, f(t, _))
  }

  object LowerPriPPrint {
    def FinalRepr[T: c0.WeakTypeTag](c0: derive.ScalaVersionStubs.Context) = c0.Expr[PPrint[T]] {
      import c0.universe._
//      println("FinalRepr " + weakTypeOf[T])
      val t = weakTypeTag[T]
      /**
        * For some reason this needs to be outside the `new Derive{...}`
        * block otherwise the Scala/Scala.js compiler crashes =/
        */
      val typeTag = c0.weakTypeTag[pprint.PPrint[_]]
      val d = new Deriver {
        val c: c0.type = c0
        def typeclass = typeTag
      }

      // Fallback manually in case everything fails
      val res = try d.derive[T] catch{
        case e if e.toString.contains("DivergentImplicit$") =>
          d.fallbackDerivation(weakTypeOf[T]) match{
            case Some(x) => x
            case None => throw e
          }
      }
      try {
        c0.typeCheck(res)
        res
      }
      catch{case e =>
        def pkg = q"_root_.pprint"
        q"""$pkg.PPrint[$t]($pkg.PPrinter.Literal)"""
      }

    }
  }
  abstract class Deriver extends Derive[pprint.PPrint]{
    import c._
    import c.universe._

    def pkg = q"_root_.pprint"
    def wrapObject(obj: Tree) = {
      q"""$pkg.PPrint[$obj.type]($pkg.PPrinter.Literal)"""
    }
    def thingy(n: Int, targetType: Type, argTypes: Seq[Type]) = {
      val toStringSymbol = targetType.member(newTermName("toString"))
      if (!toStringSymbol.isSynthetic && toStringSymbol.owner != c.weakTypeOf[Object].typeSymbol){
        fail(targetType, "LOLs")
      } else getArgSyms(targetType) match {
        case Left(msg) => fail(targetType, msg)
        case Right((companion, paramTypes, argSyms)) =>
          Seq("unapply", "unapplySeq")
            .map(newTermName(_))
            .find(companion.tpe.member(_) != NoSymbol) match {
            case None =>
              fail(
                targetType,
                "None of the following methods were defined: unapply, unapplySeq"
              )
            case Some(actionName) =>
              val t = q"$freshName"
              val cfg = q"$freshName"

              def get = q"$companion.$actionName($t).get"
              val w = n match {
                case 0 => q"()"
                case 1 => q"Tuple1($get)"
                case n => get
              }
              val tupleChunker = newTermName("Tuple" + argSyms.length + "Chunker")
              q"""
              $pkg.PPrint[$targetType](
                $pkg.PPrinter.ChunkedRepr[$targetType](
                  $pkg.Chunker[$targetType]( ($t: $targetType, $cfg: $pkg.Config) =>
                    $pkg.Chunker.$tupleChunker[..$argTypes].chunk($w, $cfg)
                  )
                )
              )
              """
          }
      }
    }
    def knot(t: Tree) = t
    def mergeTrait(subtrees: Seq[Tree], subtypes: Seq[c.Type], targetType: c.Type) = {
      val t = q"$freshName"
      val cfg = q"$freshName"
      val x = freshName
      val cases = subtrees.zip(subtypes)
                          .map{case (tree, tpe) => cq"$x: $tpe => $tree.render($x, $cfg)" }
      val finalCase =
        for(fallback <- fallbackDerivation(targetType))
        yield cq"$x => $fallback.render($x, $cfg)"
      q"""
        $pkg.PPrint[$targetType](
          $pkg.PPrinter[$targetType]{($t: $targetType, $cfg: $pkg.Config) =>
            $t match {case ..${cases ++ finalCase}}
          }
        )
      """
    }

    def wrapCase0(companion: Tree, targetType: c.Type) = thingy(0, targetType, Nil)
    def wrapCase1(companion: Tree,
                  arg: String,
                  typeArgs: Seq[c.Type],
                  argTypes: Type,
                  targetType: c.Type) = {
//      println("wrapCase1 " + typeArgs)
      thingy(1, targetType, Seq(argTypes))
    }
    def wrapCaseN(companion: Tree,
                  args: Seq[String],
                  typeArgs: Seq[c.Type],
                  argTypes: Seq[Type],
                  targetType: c.Type): Tree = {
//      println("wrapCaseN " + argTypes)
      thingy(args.length, targetType, argTypes)
    }
    override def fallbackDerivation(t: Type): Option[Tree] = Some(q"""
    $pkg.PPrint[$t]($pkg.PPrinter.Literal)
    """)
  }
}
