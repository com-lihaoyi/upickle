package upickle
import acyclic.file
import scala.reflect.ClassTag
import scala.util.{Failure, Success}
import scala.collection.SortedSet
import scala.concurrent.duration.{FiniteDuration, Duration}
import scala.reflect.macros.Context

object Implicits extends Implicits{
  def validate[T](name: String)(pf: PartialFunction[Js.Value, T]): PartialFunction[Js.Value, T] = {
    pf.orElse { case x => throw Invalid.Data(x, name) }
  }
  implicit val NothingReader: Reader[Nothing] = new ReaderCls[Nothing]({case x => ???})
  implicit val NothingWriter: Writer[Nothing] = new WriterCls[Nothing](x => ???)
  // Special-case picklers
  type JPF[T] = PartialFunction[Js.Value, T]
  val booleanReaderFunc: JPF[Boolean] = validate("Boolean"){
    case Js.True => true
    case Js.False => false
  }
  implicit object BooleanPickler extends ReadWriter[Boolean](
    if (_) Js.True else Js.False,
    booleanReaderFunc
  )

  implicit class Pipeable[T](t: T){
    def |[K](f: T => K): K = f(t)
  }
  def numericStringReaderFunc[T](func: String => T): JPF[T] = validate("Number"){
    case x: Js.String => func(x.value)
  }
  class NumericStringReadWriter[T](func: String => T) extends ReadWriter[T](
    x => Js.String(x.toString),
    numericStringReaderFunc[T](func)
  )
  def numericReaderFunc[T](func: String => T): JPF[T] = validate("Number"){
    case x: Js.Number => try{func(x.value) } catch {case e: NumberFormatException => throw Invalid.Data(x, "Number")}
    case x: Js.String => try{func(x.value) } catch {case e: NumberFormatException => throw Invalid.Data(x, "Number")}
  }

  class NumericReadWriter[T](func: String => T) extends ReadWriter[T](
    {
      case x @ Double.PositiveInfinity => Js.String(x.toString)
      case x @ Double.NegativeInfinity => Js.String(x.toString)
      case x => Js.Number(x.toString)
    },
    numericReaderFunc[T](func)
  )
  val stringReaderFunc: JPF[String] = validate("String"){
    case x: Js.String => x.value
  }
  implicit object StringPickler extends ReadWriter[String](Js.String, stringReaderFunc)

  implicit val CharPickler = new NumericStringReadWriter[Char](_(0))
  implicit val BytePickler = new NumericReadWriter(_.toByte)
  implicit val ShortPickler = new NumericReadWriter(_.toShort)
  implicit val IntPickler = new NumericReadWriter(_.toInt)
  implicit val LongPickler = new NumericReadWriter[Long](_.toLong)
  implicit val FloatPickler = new NumericReadWriter(_.toFloat)
  implicit val DoublePickler = new NumericReadWriter(_.toDouble)

  // Boilerplate tuple picklers

  implicit def Tuple1Writer[T1: Writer] = new WriterCls[Tuple1[T1]](
    x => Js.Array(Seq(writeJs(x._1)))
  )
  implicit def Tuple1Reader[T1: Reader] = new ReaderCls[Tuple1[T1]](
    validate("Array(1)"){case Js.Array(Seq(x1)) => Tuple1(readJs[T1](x1))}
  )
  implicit def Tuple2Writer[T1: Writer, T2: Writer] = new WriterCls[(T1, T2)](
    x => Js.Array(Seq(writeJs(x._1), writeJs(x._2)))
  )
  implicit def Tuple2Reader[T1: Reader, T2: Reader] = new ReaderCls[(T1, T2)](
    validate("Array(2)"){case Js.Array(Seq(x1, x2)) => (readJs[T1](x1), readJs[T2](x2))}
  )
  implicit def Tuple3Writer[T1: Writer, T2: Writer, T3: Writer] = new WriterCls[(T1, T2, T3)](
    x => Js.Array(Seq(writeJs(x._1), writeJs(x._2), writeJs(x._3)))
  )
  implicit def Tuple3Reader[T1: Reader, T2: Reader, T3: Reader] = new ReaderCls[(T1, T2, T3)](
    validate("Array(3)"){case Js.Array(Seq(x1, x2, x3)) => (readJs[T1](x1), readJs[T2](x2), readJs[T3](x3))}
  )
  implicit def Tuple4Writer[T1: Writer, T2: Writer, T3: Writer, T4: Writer] = new WriterCls[(T1, T2, T3, T4)](
    x => Js.Array(Seq(writeJs(x._1), writeJs(x._2), writeJs(x._3), writeJs(x._4)))
  )
  implicit def Tuple4Reader[T1: Reader, T2: Reader, T3: Reader, T4: Reader] = new ReaderCls[(T1, T2, T3, T4)](
    validate("Array(4)"){case Js.Array(Seq(x1, x2, x3, x4)) => (readJs[T1](x1), readJs[T2](x2), readJs[T3](x3), readJs[T4](x4))}
  )
  implicit def Tuple5Writer[T1: Writer, T2: Writer, T3: Writer, T4: Writer, T5: Writer] = new WriterCls[(T1, T2, T3, T4, T5)](
    x => Js.Array(Seq(writeJs(x._1), writeJs(x._2), writeJs(x._3), writeJs(x._4), writeJs(x._5)))
  )
  implicit def Tuple5Reader[T1: Reader, T2: Reader, T3: Reader, T4: Reader, T5: Reader] = new ReaderCls[(T1, T2, T3, T4, T5)](
    validate("Array(5)"){case Js.Array(Seq(x1, x2, x3, x4, x5)) => (readJs[T1](x1), readJs[T2](x2), readJs[T3](x3), readJs[T4](x4), readJs[T5](x5))}
  )
  implicit def Tuple6Writer[T1: Writer, T2: Writer, T3: Writer, T4: Writer, T5: Writer, T6: Writer] = new WriterCls[(T1, T2, T3, T4, T5, T6)](
    x => Js.Array(Seq(writeJs(x._1), writeJs(x._2), writeJs(x._3), writeJs(x._4), writeJs(x._5), writeJs(x._6)))
  )
  implicit def Tuple6Reader[T1: Reader, T2: Reader, T3: Reader, T4: Reader, T5: Reader, T6: Reader] = new ReaderCls[(T1, T2, T3, T4, T5, T6)](
    validate("Array(6)"){case Js.Array(Seq(x1, x2, x3, x4, x5, x6)) => (readJs[T1](x1), readJs[T2](x2), readJs[T3](x3), readJs[T4](x4), readJs[T5](x5), readJs[T6](x6))}
  )

  // Boilerplate case class pickler templates
  def Case0ReadWriter[T](t: T) = new ReadWriter[T](x => Js.Array(Nil), {case x => t})

  def Case1ReadWriter[T1: Reader: Writer, R]
                     (f: (T1) => R, g: R => Option[T1])
  = new ReadWriter[R](x => writeJs(Tuple1(g(x).get)), {case x => f(readJs[Tuple1[T1]](x)._1)})

  def Case2ReadWriter[T1: Reader: Writer, T2: Reader: Writer, R]
                     (f: (T1, T2) => R, g: R => Option[(T1, T2)])
  = new ReadWriter[R](x => writeJs(g(x).get), {case x => f.tupled(readJs[(T1, T2)](x))})

  def Case3ReadWriter[T1: Reader: Writer, T2: Reader: Writer, T3: Reader: Writer, R]
                     (f: (T1, T2, T3) => R, g: R => Option[(T1, T2, T3)])
  = new ReadWriter[R](x => writeJs(g(x).get), {case x => f.tupled(readJs[(T1, T2, T3)](x))})

  def Case4ReadWriter[T1: Reader: Writer, T2: Reader: Writer, T3: Reader: Writer, T4: Reader: Writer, R]
                     (f: (T1, T2, T3, T4) => R, g: R => Option[(T1, T2, T3, T4)])
  = new ReadWriter[R](x => writeJs(g(x).get), {case x => f.tupled(readJs[(T1, T2, T3, T4)](x))})

  def Case5ReadWriter[T1: Reader: Writer, T2: Reader: Writer, T3: Reader: Writer, T4: Reader: Writer, T5: Reader: Writer, R]
                     (f: (T1, T2, T3, T4, T5) => R, g: R => Option[(T1, T2, T3, T4, T5)])
  = new ReadWriter[R](x => writeJs(g(x).get), {case x => f.tupled(readJs[(T1, T2, T3, T4, T5)](x))})

  def Case6ReadWriter[T1: Reader: Writer, T2: Reader: Writer, T3: Reader: Writer, T4: Reader: Writer, T5: Reader: Writer, T6: Reader: Writer, R]
                     (f: (T1, T2, T3, T4, T5, T6) => R, g: R => Option[(T1, T2, T3, T4, T5, T6)])
  = new ReadWriter[R](x => writeJs(g(x).get), {case x => f.tupled(readJs[(T1, T2, T3, T4, T5, T6)](x))})

  def SeqLikeWriter[T: Writer, R[_]](g: R[T] => Option[Seq[T]]): WriterCls[R[T]] = new WriterCls[R[T]](
    x => Js.Array(g(x).get.map(x => writeJs(x)))
  )
  def SeqLikeReader[T: Reader, R[_]](f: Seq[T] => R[T]): ReaderCls[R[T]] = new ReaderCls[R[T]](
    validate("Array(n)"){case Js.Array(x) => f(x.map(readJs[T]))}
  )

  implicit def SeqWriter[T: Writer] = SeqLikeWriter[T, Seq](Seq.unapplySeq)
  implicit def SeqReader[T: Reader] = SeqLikeReader[T, Seq](Seq(_:_*))
  implicit def ListWriter[T: Writer] = SeqLikeWriter[T, List](List.unapplySeq)
  implicit def ListReader[T: Reader] = SeqLikeReader[T, List](List(_:_*))
  implicit def VectorWriter[T: Writer] = SeqLikeWriter[T, Vector](Vector.unapplySeq)
  implicit def VectorReader[T: Reader] = SeqLikeReader[T, Vector](Vector(_:_*))
  implicit def SetWriter[T: Writer] = SeqLikeWriter[T, Set](x => Some(x.toSeq))
  implicit def SetReader[T: Reader] = SeqLikeReader[T, Set](Set(_:_*))
  implicit def SortedSetWriter[T: Writer] = SeqLikeWriter[T, SortedSet](_.toSeq | Some.apply)
  implicit def SortedSetReader[T: Reader: Ordering] = SeqLikeReader[T, SortedSet](SortedSet(_:_*))

  implicit def OptionWriter[T: Writer] = SeqLikeWriter[T, Option](_.toSeq | Some.apply)
  implicit def SomeWriter[T: Writer] = new WriterCls[Some[T]](OptionWriter[T].write)
  implicit def NoneWriter = new WriterCls[None.type](OptionWriter[Int].write)
  implicit def OptionReader[T: Reader] = SeqLikeReader[T, Option](_.headOption)
  implicit def SomeReader[T: Reader] = new ReaderCls[Some[T]](OptionReader[T].read andThen (_.asInstanceOf[Some[T]]))
  implicit def NoneReader = new ReaderCls[None.type](OptionReader[Int].read andThen (_.asInstanceOf[None.type]))

  implicit def ArrayWriter[T: Writer: ClassTag] = SeqLikeWriter[T, Array](Array.unapplySeq)
  implicit def ArrayReader[T: Reader: ClassTag] = SeqLikeReader[T, Array](x => Array.apply(x:_*))

  implicit def MapWriter[K: Writer, V: Writer] = new WriterCls[Map[K, V]](
    x => Js.Array(x.toSeq.map(writeJs[(K, V)]))
  )
  implicit def MapReader[K: Reader, V: Reader] = new ReaderCls[Map[K, V]](
    validate("Array(n)"){case x: Js.Array => x.value.map(readJs[(K, V)]).toMap}
  )

  implicit val DurationWriter = new WriterCls[Duration]({
    case Duration.Inf => writeJs("inf")
    case Duration.MinusInf => writeJs("-inf")
    case x if x eq Duration.Undefined => writeJs("undef")
    case x => writeJs(x.toNanos)
  })
  implicit val InfiniteWriter = new WriterCls[Duration.Infinite](DurationWriter.write)
  implicit val InfiniteReader = new ReaderCls[Duration.Infinite]({
    case Js.String("inf") => Duration.Inf
    case Js.String("-inf") => Duration.MinusInf
    case Js.String("undef") => Duration.Undefined
  })

  implicit val FiniteWriter = new WriterCls[FiniteDuration](DurationWriter.write)
  implicit val FiniteReader = new ReaderCls[FiniteDuration]({
    case Js.Number(x) => Duration.fromNanos(x.toLong)
  })

  implicit val DurationReader = new ReaderCls[Duration](validate("DurationString"){FiniteReader.read orElse InfiniteReader.read})

  def eitherRW[T: R: W, V: R: W]: (RW[Either[T, V]], RW[Left[T, V]], RW[Right[T, V]]) = {
    knotRW{implicit i: RWKnot[Either[T, V]] => sealedRW(
      Case1ReadWriter(Left.apply[T, V], Left.unapply[T, V]),
      Case1ReadWriter(Right.apply[T, V], Right.unapply[T, V]),
      i
    )}
  }
  def ?[T](implicit t: T) = implicitly[T]
  implicit def EitherRW[A: W: R, B: W: R] = eitherRW[A, B]._1
  implicit def LeftRW[A: W: R, B: W: R] = eitherRW[A, B]._2
  implicit def RightRW[A: W: R, B: W: R] = eitherRW[A, B]._3

  def knotRW[T, V](implicit f: RWKnot[T] => V): V = f(new RWKnot(null, null))
  private[this] def annotate[V: CT](rw: ReadWriter[V], n: String) = new ReadWriter[V](
    {case x: V => Js.Array(Seq(Js.Number(n), rw.write(x)))},
    {case Js.Array(Seq(Js.Number(`n`), x)) => rw.read(x)}
  )



  private[this] implicit class mergable[T: CT, R](f: T => R){
    def merge[V: CT, U](g: V => R): U => R = {
      case v: V => g(v)
      case t: T => f(t)
    }
  }

  def sealedRW[T, A <: T: CT, B <: T: CT]
          (a: RW[A], b: RW[B], knot: RWKnot[T]) = {
    val a2 = annotate(a, "0")
    val b2 = annotate(b, "1")
    val t = new ReadWriter[T](
      a2.write merge b2.write,
      validate("Sealed"){a2.read orElse b2.read}
    )
    Option(knot).foreach(_.copyFrom(t))
    (t, a2, b2)
  }
  def sealedRW[T, A <: T: CT, B <: T: CT, C <: T: CT]
          (a: RW[A], b: RW[B], c: RW[C], knot: RWKnot[T]) = {
    val a2 = annotate(a, "0")
    val b2 = annotate(b, "1")
    val c2 = annotate(c, "2")
    val t = new ReadWriter[T](
      a2.write merge b2.write merge c2.write,
      validate("Sealed"){a2.read orElse b2.read orElse c2.read}
    )
    Option(knot).foreach(_.copyFrom(t))
    (t, a2, b2, c2)
  }
  def sealedRW[T, A <: T: CT, B <: T: CT, C <: T: CT, D <: T: CT]
          (a: RW[A], b: RW[B], c: RW[C], d: RW[D], knot: RWKnot[T]) = {
    val a2 = annotate(a, "0")
    val b2 = annotate(b, "1")
    val c2 = annotate(c, "2")
    val d2 = annotate(d, "3")
    val t = new ReadWriter[T](
      a2.write merge b2.write merge c2.write merge d2.write,
      validate("Sealed"){a2.read orElse b2.read orElse c2.read orElse d2.read}
    )
    Option(knot).foreach(_.copyFrom(t))
    (t, a2, b2, c2, d2)
  }
  def sealedRW[T, A <: T: CT, B <: T: CT, C <: T: CT, D <: T: CT, E <: T: CT]
          (a: RW[A], b: RW[B], c: RW[C], d: RW[D], e: RW[E], knot: RWKnot[T]) = {
    val a2 = annotate(a, "0")
    val b2 = annotate(b, "1")
    val c2 = annotate(c, "2")
    val d2 = annotate(d, "3")
    val e2 = annotate(e, "4")
    val t = new ReadWriter[T](
      a2.write merge b2.write merge c2.write merge d2.write merge e2.write,
      validate("Sealed"){a2.read orElse b2.read orElse c2.read orElse d2.read orElse e2.read}
    )
    Option(knot).foreach(_.copyFrom(t))
    (t, a2, b2, c2, d2, e2)
  }
  def sealedRW[T, A <: T: CT, B <: T: CT, C <: T: CT, D <: T: CT, E <: T: CT, F <: T: CT]
          (a: RW[A], b: RW[B], c: RW[C], d: RW[D], e: RW[E], f: RW[F], knot: RWKnot[T]) = {
    val a2 = annotate(a, "0")
    val b2 = annotate(b, "1")
    val c2 = annotate(c, "2")
    val d2 = annotate(d, "3")
    val e2 = annotate(e, "4")
    val f2 = annotate(f, "5")
    val t = new ReadWriter[T](
      a2.write merge b2.write merge c2.write merge d2.write merge e2.write merge f2.write,
      validate("Sealed"){a2.read orElse b2.read orElse c2.read orElse d2.read orElse e2.read orElse f2.read}
    )
    Option(knot).foreach(_.copyFrom(t))
    (t, a2, b2, c2, d2, e2, f2)
  }
  def macroRWImpl[T: c.WeakTypeTag](c: Context) = {
    println("LULS")
    import c.universe._
    val args =
      weakTypeTag[T]
        .tpe
        .decl(nme.CONSTRUCTOR)
        .asMethod
        .paramLists
        .flatten
    val params =
      args.map{m =>
        (m.name, appliedType(typeOf[RW[_]], m.typeSignature))
      }
    val name = TermName(weakTypeTag[T].tpe.typeSymbol.name.toString)

    val rwName = TermName(s"Case${params.length}ReadWriter")
    val z = q"""
      $rwName($name.apply, $name.unapply)
    """
    println("z " + z)
    c.Expr[RW[T]](z)
  }
}
import language.experimental.macros
trait Implicits {
  implicit def macroRW[T]: RW[T] = macro Implicits.macroRWImpl[T]
}
