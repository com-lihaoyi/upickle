package upickle
import acyclic.file
import scala.reflect.ClassTag
import scala.util.{Failure, Success}
import scala.collection.SortedSet
import scala.concurrent.duration.{FiniteDuration, Duration}
import scala.reflect.macros.Context
import language.experimental.macros
object Implicits extends Implicits {
  import Generated.validate
  import Generated.Tuple2Reader
  import Generated.Tuple2Writer
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


  def Case0Reader[T](t: T) = new ReaderCls[T]({case x => t})
  def Case0Writer[T](t: T) = new WriterCls[T](x => Js.Array(Nil))


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

  def knotRW[T, V](f: Knot.RW[T] => V): V = f(new Knot.RW(null, null))
  def knotR[T, V](f: Knot.R[T] => V): V = f(new Knot.R(null))
  def knotW[T, V](f: Knot.W[T] => V): V = f(new Knot.W(null))
  def annotate[V: CT](rw: Reader[V], n: String) = new ReaderCls[V](
    {case Js.Array(Seq(Js.Number(`n`), x)) => rw.read(x)}
  )
  def annotate[V: CT](rw: Writer[V], n: String) = new WriterCls[V](
    {case x: V => Js.Array(Seq(Js.Number(n), rw.write(x)))}
  )



  implicit class mergable[T: CT, R](f: T => R){
    def merge[V: CT, U](g: V => R): U => R = {
      case v: V => g(v)
      case t: T => f(t)
    }
  }

}

trait Implicits{
  implicit def macroR[T]: R[T] = macro Macros.macroRImpl[T]
  implicit def macroW[T]: W[T] = macro Macros.macroWImpl[T]
}