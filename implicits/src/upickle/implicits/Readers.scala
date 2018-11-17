package upickle.implicits

import java.util.UUID
import java.util.concurrent.TimeUnit

import upickle.core.{AbortJsonProcessingException, ArrVisitor, ObjVisitor, Util}

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.concurrent.duration.{Duration, FiniteDuration}

trait Readers extends upickle.core.Types with Generated with MacroImplicits{
  implicit object UnitReader extends Reader[Unit] {
    override def visitObject(length: Int, index: Int) = new ObjVisitor[Any, Unit] {
      def subVisitor = UnitReader

      def visitKey(s: CharSequence, index: Int): Unit = ???

      def visitValue(v: Any, index: Int): Unit = ???

      def visitEnd(index: Int) = ()
    }
  }
  implicit object BooleanReader extends Reader[Boolean] {
    override def expectedMsg = "expected boolean"
    override def visitTrue(index: Int) = true
    override def visitFalse(index: Int) = false
  }

  class FloatingNumReader[T](f1: Double => T, f2: String => T) extends Reader[T] {
    override def expectedMsg = "expected number or double string"
    override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = f2(s.toString)
    override def visitFloat64(d: Double, index: Int) = f1(d)
    override def visitString(s: CharSequence, index: Int) = f2(s.toString)
  }

  class IntegralNumReader[T](f1: Double => T, f2: Long => T) extends Reader[T] {
    override def expectedMsg = "expected number"
    override def visitFloat64(d: Double, index: Int) = f1(d)
    override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      f2(Util.parseIntegralNum(s, decIndex, expIndex, index))
    }
  }
  implicit val DoubleReader: Reader[Double] = new FloatingNumReader(identity, _.toDouble)
  implicit val IntReader: Reader[Int] = new IntegralNumReader(
    _.toInt,
    l =>
      if (l > Int.MaxValue || l < Int.MinValue) throw new AbortJsonProcessingException("expected integer")
      else l.toInt
  )
  implicit val FloatReader: Reader[Float] = new FloatingNumReader(_.toFloat, _.toFloat)
  implicit val ShortReader: Reader[Short] = new IntegralNumReader(
    _.toShort,
    l =>
      if (l > Short.MaxValue || l < Short.MinValue) throw new AbortJsonProcessingException("expected short")
      else l.toShort
  )
  implicit val ByteReader: Reader[Byte] = new IntegralNumReader(
    _.toByte,
    l =>
      if (l > Byte.MaxValue || l < Byte.MinValue) throw new AbortJsonProcessingException("expected byte")
      else l.toByte
  )

  implicit object StringReader extends Reader[String] {
    override def expectedMsg = "expected string"
    override def visitString(s: CharSequence, index: Int) = s.toString
  }
  class MapStringReader[T](f: CharSequence => T) extends Reader[T] {
    override def expectedMsg = "expected string"
    override def visitString(s: CharSequence, index: Int) = f(s)
  }

  implicit val CharReader: Reader[Char] = new MapStringReader(_.charAt(0))
  implicit val UUIDReader: Reader[UUID] = new MapStringReader(s => UUID.fromString(s.toString))
  implicit val LongReader: Reader[Long] = new MapStringReader(s => upickle.core.Util.parseLong(s, 0, s.length()))
  implicit val BigIntReader: Reader[BigInt] = new MapStringReader(s => BigInt(s.toString))
  implicit val BigDecimalReader: Reader[BigDecimal] = new MapStringReader(s => BigDecimal(s.toString))
  implicit val SymbolReader: Reader[Symbol] = new MapStringReader(s => Symbol(s.toString))

  implicit def MapReader[K, V](implicit k: Reader[K], v: Reader[V]): Reader[Map[K, V]] = {
    if (k ne StringReader) SeqLikeReader[Array, (K, V)].map(_.toMap)
    else new Reader[Map[K, V]]{
      override def visitObject(length: Int, index: Int) = new ObjVisitor[Any, Map[K, V]] {
        val strings = mutable.Buffer.empty[K]
        val values = mutable.Buffer.empty[V]
        def subVisitor = v

        def visitKey(s: CharSequence, index: Int): Unit = {
          strings.append(s.toString.asInstanceOf[K])
        }

        def visitValue(v: Any, index: Int): Unit = values.append(v.asInstanceOf[V])

        def visitEnd(index: Int) = strings.zip(values).toMap

      }
    }
  }

  implicit def OptionReader[T: Reader]: Reader[Option[T]] = SeqLikeReader[Seq, T].map(_.headOption)
  implicit def SomeReader[T: Reader]: Reader[Some[T]] = OptionReader[T].narrow[Some[T]]
  implicit def NoneReader: Reader[None.type] = OptionReader[Unit].narrow[None.type]
  implicit def SeqLikeReader[C[_], T](implicit r: Reader[T],
                                      cbf: CanBuildFrom[Nothing, T, C[T]]): Reader[C[T]] = new Reader[C[T]] {
    override def expectedMsg = "expected sequence"
    override def visitArray(length: Int, index: Int) = new ArrVisitor[Any, C[T]] {
      val b = cbf.apply()

      def visitValue(v: Any, index: Int): Unit = {
        b += v.asInstanceOf[T]
      }

      def visitEnd(index: Int) = b.result()

      def subVisitor = r
    }
  }

  implicit val DurationReader = new MapStringReader( s =>
    if (s.charAt(0) == 'i' &&
        s.charAt(1) == 'n' &&
        s.charAt(2) == 'f'
        && s.length() == 3){
      Duration.Inf
    } else if (s.charAt(0) == '-' &&
               s.charAt(1) == 'i' &&
               s.charAt(2) == 'n' &&
               s.charAt(3) == 'f' &&
               s.length() == 4){
      Duration.MinusInf
    } else if (s.charAt(0) == 'u' &&
               s.charAt(1) == 'n' &&
               s.charAt(2) == 'd' &&
               s.charAt(3) == 'e' &&
               s.charAt(4) == 'f' &&
               s.length() == 5){
      Duration.Undefined
    }else Duration(upickle.core.Util.parseLong(s, 0, s.length()), TimeUnit.NANOSECONDS)
  )

  implicit val InfiniteDurationReader = DurationReader.narrow[Duration.Infinite]
  implicit val FiniteDurationReader = DurationReader.narrow[FiniteDuration]

  implicit def EitherReader[T1: Reader, T2: Reader] = new Reader[Either[T1, T2]]{
    override def expectedMsg = "expected sequence"
    override def visitArray(length: Int, index: Int) = new ArrVisitor[Any, Either[T1, T2]] {
      var right: java.lang.Boolean = null
      var value: Either[T1, T2] = _
      def visitValue(v: Any, index: Int): Unit = right match {
        case null =>
          v match {
            case 0 => right = false
            case 1 => right = true
          }
        case java.lang.Boolean.TRUE => value = Right(v.asInstanceOf[T2])
        case java.lang.Boolean.FALSE => value = Left(v.asInstanceOf[T1])
      }

      def visitEnd(index: Int) = value

      def subVisitor = right match{
        case null => IntReader
        case java.lang.Boolean.TRUE => implicitly[Reader[T2]]
        case java.lang.Boolean.FALSE => implicitly[Reader[T1]]
      }
    }
  }
  implicit def RightReader[T1: Reader, T2: Reader] =
    EitherReader[T1, T2].narrow[Right[T1, T2]]
  implicit def LeftReader[T1: Reader, T2: Reader] =
    EitherReader[T1, T2].narrow[Left[T1, T2]]
}