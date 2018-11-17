package upickle.implicits

import java.util.UUID
import java.util.concurrent.TimeUnit

import upickle.core._

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.concurrent.duration.{Duration, FiniteDuration}

trait Readers extends upickle.core.Types with Generated with MacroImplicits{
  implicit val UnitReader = new Reader[Unit] {
    override def visitObject(length: Int, index: Int) = new ObjVisitor[Any, Unit] {
      def subVisitor = NoOpVisitor

      def visitValue(v: Any, index: Int): Unit = ()

      def visitEnd(index: Int) = ()

      def visitKey(index: Int) = NoOpVisitor

      def visitKeyValue(v: Any): Unit = ()
    }
  }
  implicit val BooleanReader = new Reader[Boolean] {
    override def expectedMsg = "expected boolean"
    override def visitTrue(index: Int) = true
    override def visitFalse(index: Int) = false
  }


  implicit val DoubleReader = new Reader[Double] {
    override def expectedMsg = "expected number"
    override def visitString(s: CharSequence, index: Int) = s.toString.toDouble
    override def visitInt32(d: Int, index: Int) = d
    override def visitInt64(d: Long, index: Int) = d
    override def visitUInt64(d: Long, index: Int) = d
    override def visitFloat64(d: Double, index: Int) = d

    override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      s.toString.toDouble
    }
  }
  implicit val IntReader = new Reader[Int] {
    override def expectedMsg = "expected number"
    override def visitInt32(d: Int, index: Int) = d
    override def visitInt64(d: Long, index: Int) = d.toInt
    override def visitUInt64(d: Long, index: Int) = d.toInt
    override def visitFloat64(d: Double, index: Int) = d.toInt
    override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      Util.parseIntegralNum(s, decIndex, expIndex, index).toInt
    }
  }
  implicit val FloatReader = new Reader[Float] {
    override def expectedMsg = "expected number"

    override def visitString(s: CharSequence, index: Int) = s.toString.toFloat
    override def visitInt32(d: Int, index: Int) = d.toFloat
    override def visitInt64(d: Long, index: Int) = d.toFloat
    override def visitUInt64(d: Long, index: Int) = d.toFloat
    override def visitFloat64(d: Double, index: Int) = d.toFloat
    override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      s.toString.toFloat
    }
  }
  implicit val ShortReader = new Reader[Short] {
    override def expectedMsg = "expected number"
    override def visitInt32(d: Int, index: Int) = d.toShort
    override def visitInt64(d: Long, index: Int) = d.toShort
    override def visitUInt64(d: Long, index: Int) = d.toShort
    override def visitFloat64(d: Double, index: Int) = d.toShort
    override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      Util.parseIntegralNum(s, decIndex, expIndex, index).toShort
    }
  }
  implicit val ByteReader = new Reader[Byte] {
    override def expectedMsg = "expected number"
    override def visitInt32(d: Int, index: Int) = d.toByte
    override def visitInt64(d: Long, index: Int) = d.toByte
    override def visitUInt64(d: Long, index: Int) = d.toByte
    override def visitFloat64(d: Double, index: Int) = d.toByte
    override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      Util.parseIntegralNum(s, decIndex, expIndex, index).toByte
    }
  }

  implicit val StringReader = new Reader[String] {
    override def expectedMsg = "expected string"
    override def visitString(s: CharSequence, index: Int) = s.toString
  }
  class MapStringReader[T](f: CharSequence => T) extends Reader[T] {
    override def expectedMsg = "expected string"
    override def visitString(s: CharSequence, index: Int) = f(s)
  }

  implicit val CharReader = new Reader[Char] {
    override def expectedMsg = "expected char"
    override def visitString(d: CharSequence, index: Int) = d.charAt(0)
    override def visitChar(d: Char, index: Int) = d
    override def visitInt32(d: Int, index: Int) = d.toChar
    override def visitInt64(d: Long, index: Int) = d.toChar
    override def visitUInt64(d: Long, index: Int) = d.toChar
    override def visitFloat64(d: Double, index: Int) = d.toChar
    override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      Util.parseIntegralNum(s, decIndex, expIndex, index).toChar
    }
  }
  implicit val UUIDReader: Reader[UUID] = new MapStringReader(s => UUID.fromString(s.toString))
  implicit val LongReader = new Reader[Long] {
    override def expectedMsg = "expected number"
    override def visitString(d: CharSequence, index: Int) = upickle.core.Util.parseLong(d, 0, d.length())
    override def visitInt32(d: Int, index: Int) = d.toLong
    override def visitInt64(d: Long, index: Int) = d.toLong
    override def visitUInt64(d: Long, index: Int) = d.toLong
    override def visitFloat64(d: Double, index: Int) = d.toLong
    override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      Util.parseIntegralNum(s, decIndex, expIndex, index).toLong
    }
  }
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

        def visitKey(index: Int) = StringReader

        def visitKeyValue(s: Any): Unit = {
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