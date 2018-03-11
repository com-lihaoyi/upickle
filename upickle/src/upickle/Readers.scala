package upickle

import java.util.UUID
import java.util.concurrent.TimeUnit

import jawn.{RawFContext, RawFacade}

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.concurrent.duration.{Duration, FiniteDuration}


trait Readers extends Types with Generated with LowPriImplicits{
  implicit object UnitReader extends Reader[Unit] {
    override def objectContext(index: Int) = new RawFContext[Any, Unit] {
      def facade = UnitReader.asInstanceOf[RawFacade[Any]]

      def visitKey(s: CharSequence, index: Int): Unit = ???

      def add(v: Any, index: Int): Unit = ???

      def finish(index: Int) = ()

      def isObj = true
    }
  }
  implicit object BooleanReader extends Reader[Boolean] {
    override def jtrue(index: Int) = true
    override def jfalse(index: Int) = false
  }

  object NumStringReader extends Reader[String] {
    override def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = s.toString
    override def jstring(s: CharSequence, index: Int) = s.toString
  }
  implicit val DoubleReader: Reader[Double] = NumStringReader.map(_.toDouble)
  implicit val IntReader: Reader[Int] = NumStringReader.map(_.toInt)
  implicit val FloatReader: Reader[Float] = NumStringReader.map(_.toFloat)
  implicit val ShortReader: Reader[Short] = NumStringReader.map(_.toShort)
  implicit val ByteReader: Reader[Byte] = NumStringReader.map(_.toByte)

  implicit object StringReader extends Reader[String] {
    override def jstring(s: CharSequence, index: Int) = s.toString
  }

  implicit val CharReader: Reader[Char] = StringReader.map(_(0))
  implicit val UUIDReader: Reader[UUID] = StringReader.map(UUID.fromString)
  implicit val LongReader: Reader[Long] = StringReader.map(_.toLong)
  implicit val BigIntReader: Reader[BigInt] = StringReader.map(BigInt(_))
  implicit val BigDecimalReader: Reader[BigDecimal] = StringReader.map(BigDecimal(_))
  implicit val SymbolReader: Reader[Symbol] = StringReader.map(Symbol.apply(_))

  implicit def MapReader[K, V](implicit k: Reader[K], v: Reader[V]): Reader[Map[K, V]] = {
    if (k eq StringReader) new BaseReader[V, Map[String, V]]{
      override def objectContext(index: Int) = new RawFContext[V, Map[String, V]] {
        val strings = mutable.Buffer.empty[String]
        val values = mutable.Buffer.empty[V]
        def facade = v

        def visitKey(s: CharSequence, index: Int): Unit = strings.append(s.toString)

        def add(v: V, index: Int): Unit = values.append(v)

        def finish(index: Int) = strings.zip(values).toMap

        def isObj = true
      }
    }.asInstanceOf[Reader[Map[K, V]]]
    else SeqLikeReader[Array, (K, V)].map(_.toMap).asInstanceOf[Reader[Map[K, V]]]
  }
  implicit def OptionReader[C[_], T: Reader]: Reader[Option[T]] = SeqLikeReader[Seq, T].map(_.headOption)
  implicit def SomeReader[C[_], T: Reader]: Reader[Some[T]] = SeqLikeReader[Seq, T].map(_.headOption.asInstanceOf[Some[T]])
  implicit def NoneReader[C[_], T: Reader]: Reader[None.type] = SeqLikeReader[Seq, T].map(_.headOption.asInstanceOf[None.type])
  implicit def SeqLikeReader[C[_], T](implicit r: Reader[T],
                                      cbf: CanBuildFrom[Nothing, T, C[T]]): Reader[C[T]] = new Reader[C[T]] {
    override def arrayContext(index: Int) = new jawn.RawFContext[Any, C[T]] {
      val b = cbf.apply()

      def visitKey(s: CharSequence, index: Int): Unit = ???

      def add(v: Any, index: Int): Unit = b += v.asInstanceOf[T]

      def finish(index: Int) = b.result()

      def isObj = false

      def facade = r.asInstanceOf[jawn.RawFacade[Any]]
    }
  }

  implicit object DurationReader extends Reader[Duration]{
    override def jstring(s: CharSequence, index: Int) = {
      s.toString match{
        case "inf" => Duration.Inf
        case "-inf" => Duration.MinusInf
        case "undef" => Duration.Undefined
        case x => Duration(x.toLong, TimeUnit.NANOSECONDS)
      }
    }
  }
  implicit val InfiniteDurationReader = DurationReader.asInstanceOf[Reader[Duration.Infinite]]
  implicit val FiniteDurationReader = DurationReader.asInstanceOf[Reader[FiniteDuration]]

  implicit def EitherReader[T1: Reader, T2: Reader] = new Reader[Either[T1, T2]]{
    override def arrayContext(index: Int) = new jawn.RawFContext[Any, Either[T1, T2]] {
      var right: java.lang.Boolean = null
      var value: Either[T1, T2] = _
      def visitKey(s: CharSequence, index: Int): Unit = ???
      def add(v: Any, index: Int): Unit = right match {
        case null =>
          v match {
            case 0 => right = false
            case 1 => right = true
          }
        case java.lang.Boolean.TRUE => value = Right(v.asInstanceOf[T2])
        case java.lang.Boolean.FALSE => value = Left(v.asInstanceOf[T1])
      }

      def finish(index: Int) = value

      def isObj = false

      def facade = right match{
        case null => IntReader.asInstanceOf[jawn.RawFacade[Any]]
        case java.lang.Boolean.TRUE => implicitly[Reader[T2]].asInstanceOf[jawn.RawFacade[Any]]
        case java.lang.Boolean.FALSE => implicitly[Reader[T1]].asInstanceOf[jawn.RawFacade[Any]]
      }
    }
  }
  implicit def RightReader[T1: Reader, T2: Reader] =
    EitherReader[T1, T2].asInstanceOf[Reader[Right[T1, T2]]]
  implicit def LeftReader[T1: Reader, T2: Reader] =
    EitherReader[T1, T2].asInstanceOf[Reader[Left[T1, T2]]]

  implicit object JsValueR extends Reader[Js.Value]{
    override def objectContext(index: Int) = {
      new RawFContext[Js.Value, Js.Obj] {
        val output = mutable.Buffer.empty[(String, Js.Value)]
        var lastKey: String = null
        def facade = JsValueR

        def visitKey(s: CharSequence, index: Int): Unit = lastKey = s.toString

        def add(v: Js.Value, index: Int): Unit = {
          output.append((lastKey, v))
        }

        def finish(index: Int) = Js.Obj(output:_*)

        def isObj = true
      }.asInstanceOf[RawFContext[Any, Js.Value]]
    }
    override def arrayContext(index: Int) = {
      new RawFContext[Js.Value, Js.Arr] {
        val output = mutable.Buffer.empty[Js.Value]
        def facade = JsValueR

        def visitKey(s: CharSequence, index: Int): Unit = ???

        def add(v: Js.Value, index: Int): Unit = {
          output.append(v)
        }

        def finish(index: Int) = Js.Arr(output:_*)

        def isObj = false
      }.asInstanceOf[RawFContext[Any, Js.Value]]
    }
    override def jstring(s: CharSequence, index: Int) = Js.Str(s)
    override def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = Js.Num(s.toString.toDouble)
    override def jtrue(index: Int) = Js.True
    override def jfalse(index: Int) = Js.False
    override def jnull(index: Int) = Js.Null
  }

  implicit def JsObjR: Reader[Js.Obj] = JsValueR.asInstanceOf[Reader[Js.Obj]]


  implicit def JsArrR: Reader[Js.Arr] = JsValueR.asInstanceOf[Reader[Js.Arr]]



  implicit def JsStrR: Reader[Js.Str] = JsValueR.asInstanceOf[Reader[Js.Str]]


  implicit def JsNumR: Reader[Js.Num] = JsValueR.asInstanceOf[Reader[Js.Num]]


  implicit def JsTrueR: Reader[Js.True.type] = JsValueR.asInstanceOf[Reader[Js.True.type]]

  implicit def JsFalseR: Reader[Js.False.type] = JsValueR.asInstanceOf[Reader[Js.False.type]]


  implicit def JsNullR: Reader[Js.Null.type] = JsValueR.asInstanceOf[Reader[Js.Null.type]]
}