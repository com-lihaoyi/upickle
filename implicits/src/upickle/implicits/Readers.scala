package upickle.implicits

import java.util.UUID
import java.util.concurrent.TimeUnit

import upickle.core._

import upickle.core.compat._
import scala.collection.mutable
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.reflect.ClassTag

trait Readers extends upickle.core.Types
  with Generated
  with ReadersVersionSpecific { this: Annotator =>
  implicit val UnitReader: Reader[Unit] = new SimpleReader[Unit] with MapKey{
    override def expectedMsg = "expected unit"
    override def visitObject(length: Int, index: Int) = new ObjVisitor[Any, Unit] {
      def subVisitor = NoOpVisitor

      def visitValue(v: Any, index: Int): Unit = ()

      def visitEnd(index: Int) = ()

      def visitKey(index: Int) = NoOpVisitor

      def visitKeyValue(v: Any): Unit = ()
    }

    override def visitNull(index: Int): Unit = ()
  }

  implicit val BooleanReader: Reader[Boolean] = new SimpleReader[Boolean] with MapKey{
    override def expectedMsg = "expected boolean"
    override def visitTrue(index: Int) = true
    override def visitFalse(index: Int) = false

    override def visitString(s: CharSequence, index: Int) = s.toString.toBoolean
  }

  protected trait NumericReader[T] extends SimpleReader[T] {
    override def visitFloat64String(s: String, index: Int) = {
      visitFloat64StringParts(
        s,
        s.indexOf('.'),
        s.indexOf('E') match{
          case -1 => s.indexOf('e')
          case n => n
        },
        -1
      )
    }
  }

  implicit val DoubleReader: Reader[Double] = new NumericReader[Double] with MapKey{
    override def expectedMsg = "expected number"
    override def visitString(s: CharSequence, index: Int) = s.toString.toDouble
    override def visitInt32(d: Int, index: Int) = d
    override def visitInt64(d: Long, index: Int) = d
    override def visitUInt64(d: Long, index: Int) = d
    override def visitFloat32(d: Float, index: Int) = d
    override def visitFloat64(d: Double, index: Int) = d
    override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      s.toString.toDouble
    }

  }
  implicit val IntReader: Reader[Int] = new NumericReader[Int] with MapKey{
    override def expectedMsg = "expected number"
    override def visitString(s: CharSequence, index: Int) = s.toString.toInt
    override def visitInt32(d: Int, index: Int) = d
    override def visitInt64(d: Long, index: Int) = d.toInt
    override def visitUInt64(d: Long, index: Int) = d.toInt
    override def visitFloat32(d: Float, index: Int) = d.toInt
    override def visitFloat64(d: Double, index: Int) = d.toInt
    override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      Util.parseIntegralNum(s, decIndex, expIndex, index).toInt
    }

  }

  implicit val FloatReader: Reader[Float] = new NumericReader[Float] with MapKey{
    override def expectedMsg = "expected number"

    override def visitString(s: CharSequence, index: Int) = s.toString.toFloat
    override def visitInt32(d: Int, index: Int) = d.toFloat
    override def visitInt64(d: Long, index: Int) = d.toFloat
    override def visitUInt64(d: Long, index: Int) = d.toFloat
    override def visitFloat32(d: Float, index: Int) = d
    override def visitFloat64(d: Double, index: Int) = d.toFloat
    override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      s.toString.toFloat
    }

  }

  implicit val ShortReader: Reader[Short] = new NumericReader[Short] with MapKey{
    override def expectedMsg = "expected number"
    override def visitString(s: CharSequence, index: Int) = s.toString.toShort
    override def visitInt32(d: Int, index: Int) = d.toShort
    override def visitInt64(d: Long, index: Int) = d.toShort
    override def visitUInt64(d: Long, index: Int) = d.toShort
    override def visitFloat32(d: Float, index: Int) = d.toShort
    override def visitFloat64(d: Double, index: Int) = d.toShort
    override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      Util.parseIntegralNum(s, decIndex, expIndex, index).toShort
    }

  }

  implicit val ByteReader: Reader[Byte] = new NumericReader[Byte] with MapKey{
    override def expectedMsg = "expected number"
    override def visitString(s: CharSequence, index: Int) = s.toString.toByte
    override def visitInt32(d: Int, index: Int) = d.toByte
    override def visitInt64(d: Long, index: Int) = d.toByte
    override def visitUInt64(d: Long, index: Int) = d.toByte
    override def visitFloat32(d: Float, index: Int) = d.toByte
    override def visitFloat64(d: Double, index: Int) = d.toByte
    override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      Util.parseIntegralNum(s, decIndex, expIndex, index).toByte
    }

  }

  implicit val StringReader: Reader[String] = new SimpleReader[String] with MapKey{
    override def expectedMsg = "expected string"
    override def visitString(s: CharSequence, index: Int) = s.toString

  }


  trait SimpleMapStringReader[T] extends SimpleReader[T] with MapKey{
    override def expectedMsg = "expected string"
    override def visitString(s: CharSequence, index: Int) = readString(s)
    def readString(s: CharSequence): T
  }

  implicit val CharReader: Reader[Char] = new NumericReader[Char] with MapKey{
    override def expectedMsg = "expected char"
    override def visitString(d: CharSequence, index: Int) = d.charAt(0)
    override def visitChar(d: Char, index: Int) = d
    override def visitInt32(d: Int, index: Int) = d.toChar
    override def visitInt64(d: Long, index: Int) = d.toChar
    override def visitUInt64(d: Long, index: Int) = d.toChar
    override def visitFloat32(d: Float, index: Int) = d.toChar
    override def visitFloat64(d: Double, index: Int) = d.toChar
    override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      Util.parseIntegralNum(s, decIndex, expIndex, index).toChar
    }

  }

  implicit val UUIDReader: Reader[UUID] = new SimpleMapStringReader[UUID]{
    def readString(s: CharSequence) = UUID.fromString(s.toString)
  }

  implicit val LongReader: Reader[Long] = new NumericReader[Long] with MapKey{
    override def expectedMsg = "expected number"
    override def visitString(d: CharSequence, index: Int) = upickle.core.Util.parseLong(d, 0, d.length())
    override def visitInt32(d: Int, index: Int) = d.toLong
    override def visitInt64(d: Long, index: Int) = d.toLong
    override def visitUInt64(d: Long, index: Int) = d.toLong
    override def visitFloat32(d: Float, index: Int) = d.toLong
    override def visitFloat64(d: Double, index: Int) = d.toLong
    override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      Util.parseIntegralNum(s, decIndex, expIndex, index).toLong
    }

  }

  implicit val BigIntReader: Reader[BigInt] = new SimpleMapStringReader[BigInt]{
    def readString(s: CharSequence) = BigInt(s.toString)
  }
  implicit val BigDecimalReader: Reader[BigDecimal] = new SimpleMapStringReader[BigDecimal]{
    def readString(s: CharSequence) = BigDecimal(s.toString)
  }
  implicit val SymbolReader: Reader[Symbol] = new SimpleMapStringReader[Symbol]{
    def readString(s: CharSequence) = Symbol(s.toString)
  }



  def MapReader0[M[A, B] <: collection.Map[A, B], K, V]
                (make: Iterable[(K, V)] => M[K, V])
                (implicit k: Reader[K], v: Reader[V]): Reader[M[K, V]] = {
    k match{
      case k: Reader[K] with MapKey =>
        new SimpleReader[M[K, V]]{
          override def visitObject(length: Int, index: Int) = new ObjVisitor[Any, M[K, V]] {
            val keys = mutable.Buffer.empty[K]
            val values = mutable.Buffer.empty[V]
            def subVisitor = v

            def visitKey(index: Int) = k

            def visitKeyValue(s: Any): Unit = keys.append(s.asInstanceOf[K])

            def visitValue(v: Any, index: Int): Unit = values.append(v.asInstanceOf[V])

            def visitEnd(index: Int) = make(keys.zip(values))
          }

          override def visitArray(length: Int, index: Int) = {
            SeqLikeReader[Array, (K, V)](Tuple2Reader(k, v), implicitly)
              .map(x => make(x))
              .visitArray(length, index)
          }

          def expectedMsg = "expected map or sequence"
        }

      case _ => SeqLikeReader[Array, (K, V)].map(x => make(x))
    }

  }

  implicit def MapReader1[K, V](implicit k: Reader[K], v: Reader[V]): Reader[collection.Map[K, V]] = {
    MapReader0[collection.Map, K, V](_.toMap)
  }
  implicit def MapReader2[K, V](implicit k: Reader[K], v: Reader[V]): Reader[collection.immutable.Map[K, V]] = {
    MapReader0[collection.immutable.Map, K, V]{seq =>
      val b = collection.immutable.Map.newBuilder[K, V]
      seq.foreach(b += _)
      b.result()
    }
  }
  implicit def MapReader3[K, V](implicit k: Reader[K], v: Reader[V]): Reader[collection.mutable.Map[K, V]] = {
    MapReader0[collection.mutable.Map, K, V]{seq =>
      val b = collection.mutable.Map.newBuilder[K, V]
      seq.foreach(b += _)
      b.result()
    }
  }

  implicit def OptionReader[T: Reader]: Reader[Option[T]] = new SimpleReader[Option[T]] {
    override def expectedMsg = "expected sequence"
    override def visitArray(length: Int, index: Int) = new ArrVisitor[Any, Option[T]] {
      var b: Option[T] = None

      def visitValue(v: Any, index: Int): Unit = {
        b = Some(v.asInstanceOf[T])
      }

      def visitEnd(index: Int) = b

      def subVisitor = implicitly[Reader[T]]
    }
  }
  implicit def SomeReader[T: Reader]: Reader[Some[T]] = OptionReader[T].narrow[Some[T]]
  implicit def NoneReader: Reader[None.type] = OptionReader[Unit].narrow[None.type]

  implicit def ArrayReader[T: Reader: ClassTag]: Reader[Array[T]] =
    if (implicitly[Reader[T]] == ByteReader) new SimpleReader[Array[T]] {
      override def expectedMsg = "expected sequence"

      override def visitBinary(bytes: Array[Byte], offset: Int, len: Int, index: Int) = {
        bytes.slice(offset, offset + len).asInstanceOf[Array[T]]
      }
      override def visitArray(length: Int, index: Int) = new ArrVisitor[Any, Array[T]] {
        val b = mutable.ArrayBuilder.make[T]

        def visitValue(v: Any, index: Int): Unit = {
          b += v.asInstanceOf[T]
        }

        def visitEnd(index: Int) = b.result()

        def subVisitor = implicitly[Reader[T]]
      }
    }
    else new SimpleReader[Array[T]] {
      override def expectedMsg = "expected sequence"
      override def visitArray(length: Int, index: Int) = new ArrVisitor[Any, Array[T]] {
        val b = mutable.ArrayBuilder.make[T]

        def visitValue(v: Any, index: Int): Unit = {
          b += v.asInstanceOf[T]
        }

        def visitEnd(index: Int) = b.result()

        def subVisitor = implicitly[Reader[T]]
      }
    }
  implicit def SeqLikeReader[C[_], T](implicit r: Reader[T],
                                      factory: Factory[T, C[T]]): Reader[C[T]] = new SimpleReader[C[T]] {
    override def expectedMsg = "expected sequence"
    override def visitArray(length: Int, index: Int) = new ArrVisitor[Any, C[T]] {
      val b = factory.newBuilder

      def visitValue(v: Any, index: Int): Unit = {
        b += v.asInstanceOf[T]
      }

      def visitEnd(index: Int) = b.result()

      def subVisitor = r
    }
  }

  implicit val DurationReader: Reader[Duration] = new SimpleMapStringReader[Duration]{
    override def readString(s: CharSequence) =
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

  }

  implicit val InfiniteDurationReader: Reader[Duration.Infinite] = DurationReader.narrow[Duration.Infinite]
  implicit val FiniteDurationReader: Reader[FiniteDuration] = DurationReader.narrow[FiniteDuration]

  implicit def EitherReader[T1: Reader, T2: Reader]: SimpleReader[Either[T1, T2]] = new SimpleReader[Either[T1, T2]]{
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

      def subVisitor: Visitor[_, _] = right match{
        case null => IntReader
        case java.lang.Boolean.TRUE => implicitly[Reader[T2]]
        case java.lang.Boolean.FALSE => implicitly[Reader[T1]]
      }
    }
  }
  implicit def RightReader[T1: Reader, T2: Reader]: Reader[Right[T1, T2]] =
    EitherReader[T1, T2].narrow[Right[T1, T2]]
  implicit def LeftReader[T1: Reader, T2: Reader]: Reader[Left[T1, T2]] =
    EitherReader[T1, T2].narrow[Left[T1, T2]]
}
