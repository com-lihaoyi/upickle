package upickle
import scala.reflect.ClassTag
import scala.util.{Failure, Success}
import scala.collection.SortedSet
import scala.concurrent.duration.{FiniteDuration, Duration}


trait Implicits {
  // Special-case picklers
  implicit object BooleanPickler extends ReadWriter[Boolean](
    if (_) Js.True else Js.False,
    _ match{
      case Js.True => true
      case Js.False => false
    }
  )

  implicit class Pipeable[T](t: T){
    def |[K](f: T => K): K = f(t)
  }
  class NumericStringReadWriter[T](func: String => T) extends ReadWriter[T](
    x => Js.String(x.toString),
    x => func(x.asInstanceOf[Js.String].s)
  )
  class NumericReadWriter[T](func: String => T) extends ReadWriter[T](
    x => Js.Number(x.toString),
    x => func(x.asInstanceOf[Js.Number].d)
  )

  implicit object StringPickler extends ReadWriter[String](Js.String, _.asInstanceOf[Js.String].s)
  implicit val NothingReader = new ReaderCls[Nothing](x => ???)
  implicit val NothingWriter= new WriterCls[Nothing](x => ???)
  implicit val CharPickler = new NumericStringReadWriter[Char](_(0))
  implicit val BytePickler = new NumericReadWriter(_.toByte)
  implicit val ShortPickler = new NumericReadWriter(_.toShort)
  implicit val IntPickler = new NumericReadWriter(_.toInt)
  implicit val LongPickler = new NumericStringReadWriter[Long](_.toLong)
  implicit val FloatPickler = new NumericStringReadWriter(_.toFloat)
  implicit val DoublePickler = new NumericStringReadWriter(_.toDouble)

  // Boilerplate tuple picklers

  implicit def Tuple1Writer[T1: Writer] = new WriterCls[Tuple1[T1]](
    x => Js.Array(Seq(writeJs(x._1)))
  )
  implicit def Tuple1Reader[T1: Reader] = new ReaderCls[Tuple1[T1]]({
    case Js.Array(Seq(x1)) => Tuple1(readJs[T1](x1))
  })
  implicit def Tuple2Writer[T1: Writer, T2: Writer] = new WriterCls[(T1, T2)](
    x => Js.Array(Seq(writeJs(x._1), writeJs(x._2)))
  )
  implicit def Tuple2Reader[T1: Reader, T2: Reader] = new ReaderCls[(T1, T2)]({
    case Js.Array(Seq(x1, x2)) => (readJs[T1](x1), readJs[T2](x2))
    case x => println(x); throw new Exception()
  })
  implicit def Tuple3Writer[T1: Writer, T2: Writer, T3: Writer] = new WriterCls[(T1, T2, T3)](
    x => Js.Array(Seq(writeJs(x._1), writeJs(x._2), writeJs(x._3)))
  )
  implicit def Tuple3Reader[T1: Reader, T2: Reader, T3: Reader] = new ReaderCls[(T1, T2, T3)]({
    case Js.Array(Seq(x1, x2, x3)) => (readJs[T1](x1), readJs[T2](x2), readJs[T3](x3))
  })
  implicit def Tuple4Writer[T1: Writer, T2: Writer, T3: Writer, T4: Writer] = new WriterCls[(T1, T2, T3, T4)](
    x => Js.Array(Seq(writeJs(x._1), writeJs(x._2), writeJs(x._3), writeJs(x._4)))
  )
  implicit def Tuple4Reader[T1: Reader, T2: Reader, T3: Reader, T4: Reader] = new ReaderCls[(T1, T2, T3, T4)]({
    case Js.Array(Seq(x1, x2, x3, x4)) => (readJs[T1](x1), readJs[T2](x2), readJs[T3](x3), readJs[T4](x4))
  })
  implicit def Tuple5Writer[T1: Writer, T2: Writer, T3: Writer, T4: Writer, T5: Writer] = new WriterCls[(T1, T2, T3, T4, T5)](
    x => Js.Array(Seq(writeJs(x._1), writeJs(x._2), writeJs(x._3), writeJs(x._4), writeJs(x._5)))
  )
  implicit def Tuple5Reader[T1: Reader, T2: Reader, T3: Reader, T4: Reader, T5: Reader] = new ReaderCls[(T1, T2, T3, T4, T5)]({
    case Js.Array(Seq(x1, x2, x3, x4, x5)) => (readJs[T1](x1), readJs[T2](x2), readJs[T3](x3), readJs[T4](x4), readJs[T5](x5))
  })
  implicit def Tuple6Writer[T1: Writer, T2: Writer, T3: Writer, T4: Writer, T5: Writer, T6: Writer] = new WriterCls[(T1, T2, T3, T4, T5, T6)](
    x => Js.Array(Seq(writeJs(x._1), writeJs(x._2), writeJs(x._3), writeJs(x._4), writeJs(x._5), writeJs(x._6)))
  )
  implicit def Tuple6Reader[T1: Reader, T2: Reader, T3: Reader, T4: Reader, T5: Reader, T6: Reader] = new ReaderCls[(T1, T2, T3, T4, T5, T6)]({
    case Js.Array(Seq(x1, x2, x3, x4, x5, x6)) => (readJs[T1](x1), readJs[T2](x2), readJs[T3](x3), readJs[T4](x4), readJs[T5](x5), readJs[T6](x6))
  })

  // Boilerplate case class pickler templates

  def Case1ReadWriter[T1: Reader: Writer, R]
  (f: (T1) => R, g: R => Option[T1])
  = new ReadWriter[R](x => writeJs(Tuple1(g(x).get)), x => f(readJs[Tuple1[T1]](x)._1))

  def Case2ReadWriter[T1: Reader: Writer, T2: Reader: Writer, R]
  (f: (T1, T2) => R, g: R => Option[(T1, T2)])
  = new ReadWriter[R](x => writeJs(g(x).get), x => f.tupled(readJs[(T1, T2)](x)))

  def Case3ReadWriter[T1: Reader: Writer, T2: Reader: Writer, T3: Reader: Writer, R]
  (f: (T1, T2, T3) => R, g: R => Option[(T1, T2, T3)])
  = new ReadWriter[R](x => writeJs(g(x).get), x => f.tupled(readJs[(T1, T2, T3)](x)))

  def Case4ReadWriter[T1: Reader: Writer, T2: Reader: Writer, T3: Reader: Writer, T4: Reader: Writer, R]
  (f: (T1, T2, T3, T4) => R, g: R => Option[(T1, T2, T3, T4)])
  = new ReadWriter[R](x => writeJs(g(x).get), x => f.tupled(readJs[(T1, T2, T3, T4)](x)))

  def Case5ReadWriter[T1: Reader: Writer, T2: Reader: Writer, T3: Reader: Writer, T4: Reader: Writer, T5: Reader: Writer, R]
  (f: (T1, T2, T3, T4, T5) => R, g: R => Option[(T1, T2, T3, T4, T5)])
  = new ReadWriter[R](x => writeJs(g(x).get), x => f.tupled(readJs[(T1, T2, T3, T4, T5)](x)))

  def Case6ReadWriter[T1: Reader: Writer, T2: Reader: Writer, T3: Reader: Writer, T4: Reader: Writer, T5: Reader: Writer, T6: Reader: Writer, R]
  (f: (T1, T2, T3, T4, T5, T6) => R, g: R => Option[(T1, T2, T3, T4, T5, T6)])
  = new ReadWriter[R](x => writeJs(g(x).get), x => f.tupled(readJs[(T1, T2, T3, T4, T5, T6)](x)))

  def SeqLikeWriter[T: Writer, R[_]](g: R[T] => Option[Seq[T]]): WriterCls[R[T]] = new WriterCls[R[T]](
    x => Js.Array(g(x).get.map(x => writeJs(x)))
  )
  def SeqLikeReader[T: Reader, R[_]](f: Seq[T] => R[T]): ReaderCls[R[T]] = new ReaderCls[R[T]](
    x => f(x.asInstanceOf[Js.Array].args.map(readJs[T]))
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
  implicit def SomeReader[T: Reader] = new ReaderCls[Some[T]](OptionReader[T].read(_).asInstanceOf[Some[T]])
  implicit def NoneReader = new ReaderCls[None.type](OptionReader[Int].read(_).asInstanceOf[None.type])

  implicit def ArrayWriter[T: Writer: ClassTag] = SeqLikeWriter[T, Array](Array.unapplySeq)
  implicit def ArrayReader[T: Reader: ClassTag] = SeqLikeReader[T, Array](x => Array.apply(x:_*))

  implicit def MapWriter[K: Writer, V: Writer] = new WriterCls[Map[K, V]](
    x => Js.Array(x.toSeq.map(writeJs[(K, V)]))
  )
  implicit def MapReader[K: Reader, V: Reader] = new ReaderCls[Map[K, V]](
    x => x.asInstanceOf[Js.Array].args.map(readJs[(K, V)]).toMap
  )

  implicit val DurationWriter = new WriterCls[Duration]({
    case Duration.Inf => writeJs("inf")
    case Duration.MinusInf => writeJs("-inf")
    case x if x eq Duration.Undefined => writeJs("undef")
    case x => writeJs(x.toNanos)
  })
  implicit val DurationReader= new ReaderCls[Duration]({
    case Js.String("inf") => Duration.Inf
    case Js.String("-inf") => Duration.MinusInf
    case Js.String("undef") => Duration.Undefined
    case Js.String(x) => Duration.fromNanos(x.toLong)
  })
  implicit val InfiniteWriter = new WriterCls[Duration.Infinite](DurationWriter.write)
  implicit val InfiniteReader = new ReaderCls[Duration.Infinite](
    x => DurationReader.read(x).asInstanceOf[Duration.Infinite]
  )

  implicit val FiniteWriter = new WriterCls[FiniteDuration](DurationWriter.write)
  implicit val FiniteReader = new ReaderCls[FiniteDuration](
    x => DurationReader.read(x).asInstanceOf[FiniteDuration]
  )

  implicit def EitherWriter[A: Writer, B: Writer] = new WriterCls[Either[A, B]]({
    case Left(a) => Js.Array(Seq(Js.Number("0"), writeJs(a)))
    case Right(b) => Js.Array(Seq(Js.Number("1"), writeJs(b)))
  })

  implicit def EitherReader[A: Reader, B: Reader] = new ReaderCls[Either[A, B]]({
    case Js.Array(Seq(Js.Number("0"), a)) => Left(readJs[A](a))
    case Js.Array(Seq(Js.Number("1"), b)) => Right(readJs[B](b))
  })
  implicit def LeftWriter[A: Writer] = new WriterCls[Left[A, Nothing]]({
    case Left(a) => Js.Array(Seq(Js.Number("0"), writeJs(a)))
  })
  implicit def LeftReader[A: Reader] = new ReaderCls[Left[A, Nothing]]({
    case Js.Array(Seq(Js.Number("0"), a)) => Left(readJs[A](a))
  })
  implicit def RightWriter[B: Writer] = new WriterCls[Right[Nothing, B]]({
    case Right(b) => Js.Array(Seq(Js.Number("1"), writeJs(b)))
  })
  implicit def RightReader[B: Reader] = new ReaderCls[Right[Nothing, B]]({
    case Js.Array(Seq(Js.Number("1"), b)) => Right(readJs[B](b))
  })
}