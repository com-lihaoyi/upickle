package pprint

import derive.Derive

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.generic.CanBuildFrom
import scala.language.experimental.macros
import annotation.tailrec
import acyclic.file
import scala.{Iterator => Iter}
import compat._
object PPrint extends Internals.LowPriPPrint{

  /**
   * Prettyprint a strongly-typed value, falling back to toString
   * if you don't know what to do with it. Generally used for human-facing
   * output
   */
  def apply[T: PPrint](t: T): Iter[String] = {
    val pprint = implicitly[PPrint[T]]
    pprint.render(t)
  }

  def apply[A](pprinter0: PPrinter[A], cfg: Config) = new PPrint[A] {
    val pprinter = pprinter0
    def render(t: A): Iter[String] = {
      if (t == null) Iter("null")
      else pprinter.render(t, cfg)
    }
  }


  type PPrint[A] = pprint.PPrint[A]
  object Knot {

    case class PPrint[A](f0: () => pprint.PPrint[A]) extends pprint.PPrint[A] {
      lazy val f = f0()

      def render(t: A) = f.render(t)

      def pprinter = f.pprinter
    }

  }
  /**
   * Helper to make implicit resolution behave right
   */
  implicit def Contra[A](implicit ca: PPrinter[A], cfg: Config): PPrint[A] =
    PPrint(ca, cfg)

}


/**
 * A typeclass necessary to prettyprint something. Separate from [[PPrinter]]
 * in order to make contravariant implicit resolution behave right.
 */
trait PPrint[A]{
  def render(t: A): Iter[String]
  def pprinter: PPrinter[A]
  def map(f: String => String) = pprinter.map(f)
}

/**
 * Wrapper type for disabling output truncation.
 * PPrint(Full(value)) will always return the full output.
 */
case class Show[A](value: A, lines: Int)

/**
 * A typeclass you define to prettyprint values of type [[A]]
 */
trait PPrinter[-A] {
  def render(t: A, c: Config): Iter[String]

  def map(f: String => String): PPrinter[A] = PPrinter {
    (t: A, c: Config) => render(t, c).map(f)
  }  
}

object PPrinter extends PPrinterGen with LowPriPPrinter{
  // Things being injected into PPrinterGen to keep it acyclic
  type UP[T] = Internals.Unpacker[T]
  type PP[T] = PPrint[T]
  type C = Config
  type PPrinter[T] = pprint.PPrinter[T]
  /**
   * Special, because `Product0` doesn't exist
   */
  implicit def Product0Unpacker = (t: Unit) => Iter[Iter[String]]()
  def makePPrinter[T](f: (T, Config) => Iter[Iter[String]]) = apply(
    (t: T, c: Config) =>
      Internals.handleChunks("", c, c => f(t, c))

  )
  def render[T: PP](t: T, c: Config) = implicitly[PPrint[T]].pprinter.render(t, c)

  def apply[T](r: (T, Config) => Iter[String]): PPrinter[T] = {
    new PPrinter[T]{ 
      def render(t: T, c: Config) = {
        if(c.lines() > 0)
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
  def literalColorPPrinter[T]: PPrinter[T] = PPrinter[T] { (t: T, c: Config) =>
    Iter(c.color.literal("" + t))
  }

  implicit val UnitRepr = literalColorPPrinter[Unit]
//  implicit val NullRepr = literalColorPPrinter[Null]
  implicit val ByteRepr = literalColorPPrinter[Byte]
  implicit val ShortRepr = literalColorPPrinter[Short]
  implicit val IntRepr = literalColorPPrinter[Int]
  implicit val LongRepr = literalColorPPrinter[Long].map(_+"L")
  implicit val FloatRepr = literalColorPPrinter[Float].map(_+"F")
  implicit val DoubleRepr = literalColorPPrinter[Double]
  implicit val CharRepr = PPrinter[Char] { (x, c) =>
    val body = Iter("'", escape(x.toString), "'")
    if (c.literalColor == null) body
    else Iter(c.literalColor) ++ body ++ Iter(Console.RESET)
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

    if (c.literalColor == null) body
    else Iter(c.literalColor) ++ body ++ Iter(Console.RESET)
  }
  implicit val SymbolRepr = PPrinter[Symbol]((x, c) =>
    Iter(c.color.literal("'" + x.name))
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

  private def takeFirstLines(cfg: Config, iter: Iter[String]): Iter[String] = {
   
    //Calculates how many lines and characters are remaining after printing the given string.
    //Also returns how much of thsi string can be printed if the space runs out
    @tailrec
    def charIter(str: String, pos: Int, lines: Int, chars: Int): (Int, Int, Option[Int]) = {
      if(pos >= str.length) (lines, chars, None)
      else if(lines == 1 && chars == 0){
        //this would be the first character wrapping into the first line not printed
        (0, 0, Some(pos))
      }
      else{

        val (remainingLines, remainingChars) =
          if(str(pos) == '\n') (lines - 1, cfg.maxWidth()) //starting a new line
          else if(chars == 0) (lines - 1, cfg.maxWidth() - 1) //wrapping around and printing a character
          else (lines, chars - 1) //simply printing a character
        if(remainingLines == 0) (lines, chars, Some(pos + 1))
        else charIter(str, pos + 1, remainingLines, remainingChars)
      }
    }
   
    @tailrec
    def strIter(lines: Int, chars: Int, begin: Iter[String]): Iter[String] = {
      if(!iter.hasNext) begin
      else if(lines == 0) begin ++ Iter(cfg.color.prefix("..."))
      else{
        val head = iter.next
        val (remainingLines, remainingChars, substringLength) = charIter(head, 0, lines, chars)
        if(!substringLength.isEmpty){
          begin ++ Iter(
            head.substring(0, substringLength.get),
            cfg.color.prefix("...")
          )
        } else {
          strIter(remainingLines, remainingChars, begin ++ Iter(head))
        }
      }
    }
    strIter(cfg.lines(), cfg.maxWidth(), Iter.empty)
  }

  implicit def ArrayRepr[T: PPrint] = PPrinter[Array[T]]{
    def repr = Internals.collectionRepr[T, Seq[T]]
    (t: Array[T], c: Config) => repr.render(t, c)
  }



  implicit def MapRepr[T: PPrint, V: PPrint] = Internals.makeMapRepr[collection.Map, T, V]

  implicit def showPPrinter[A: PPrint]: PPrinter[Show[A]] = {
    new PPrinter[Show[A]]{
      def render(wrapper: Show[A], c: Config) = {
        implicitly[PPrint[A]].pprinter.render(
          wrapper.value,
          c.copy(lines = () => wrapper.lines)
        )
      }
    }
  }
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
    val coloredName = c.color.prefix(renamed)
    // Prefix, contents, and all the extra ", " "(" ")" characters
    val horizontalChunks =
      chunkFunc(c).flatMap(", " +: _.toStream)
                  .toStream
                  .drop(1)
    val effectiveWidth = c.maxWidth() - (c.depth * c.indent)
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
    else Iter(coloredName, "(") ++ horizontalChunks ++ Iter(")")
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
    val renamed = c.rename(name)
    val coloredName = c.color.prefix(renamed)
    val chunks2 = chunkFunc(c.deeper)

    // Needs to be a def to avoid exhaustion
    def indent = Iter.fill(c.depth)("  ")

    Iter(coloredName, "(\n") ++
    chunks2.flatMap(Iter(",\n", "  ") ++ indent ++ _).drop(1) ++
    Iter("\n") ++ indent ++ Iter(")")
  }

  type Unpacker[T] = (T, Config) => Iter[Iter[String]]


  trait LowPriPPrint {
//    implicit def FinalRepr[T]: PPrint[T] = macro LowerPriPPrint.FinalRepr[T]
  }

  def fromUnpacker[T](prefix: T => String)(f: Internals.Unpacker[T]): PPrinter[T] = PPrinter[T]{
    (t: T, c: Config) => Internals.handleChunks(prefix(t), c, f(t, _))
  }

  object LowerPriPPrint {
//    def fromUnpackerTwo[T <: Product](f: (T, Config) => Iter[Iter[String]])(implicit cfg: ammonite.pprint.Config) = {
//      ammonite.pprint.PPrint[T](
//        ammonite.pprint.Internals.fromUnpacker[T](_.productPrefix){
//          f
//        },
//        implicitly[Config]
//      )
//    }
//    def FinalRepr[T: c0.WeakTypeTag](c0: derive.ScalaVersionStubs.Context) = c0.Expr[PPrint[T]] {
//      import c0.universe._
//      println("FinalRepr " + weakTypeOf[T])
//      val R = new Derive{
//        def typeclassName = "PPrint"
//        def wrapObject(t: Tree): Tree
//        def wrapCase0(t: Tree, targetType: c.Type): Tree
//        def wrapCase1(t: Tree, arg: String, default: Tree, typeArgs: Seq[c.Type], argTypes: Type, targetType: c.Type): Tree
//        def wrapCaseN(t: Tree, args: Seq[String], defaults: Seq[Tree], typeArgs: Seq[c.Type], argTypes: Seq[Type],targetType: c.Type): Tree
//      }
//      Config(
//        "PPrint",
//        "PPrint",
//        Seq("unapply", "unapplySeq"),
//        false,
//        false,
//        n => s"Unpacker.Product${n}Unpacker"
//      )
//
//      val res = new Derive(R){val c: c0.type = c0}.derive[T](
//        _.map(p => q"$p.read": Tree)
//          .reduce((a, b) => q"$a orElse $b")
//      )(implicitly[c0.WeakTypeTag[T]])
//      println(res)
//      res
//    }
  }

}
