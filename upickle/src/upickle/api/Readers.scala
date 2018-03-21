package upickle
package api

import java.util.UUID
import java.util.concurrent.TimeUnit

import upickle.internal.IndexedJs
import upickle.jawn.{AbortJsonProcessingException, ArrVisitor, ObjArrVisitor, ObjVisitor}

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.concurrent.duration.{Duration, FiniteDuration}


trait Readers extends upickle.core.Types with Generated with MacroImplicits{
  implicit object UnitReader extends Reader[Unit] {
    override def visitObject(index: Int) = new ObjVisitor[Any, Unit] {
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

  object NumStringReader extends Reader[String] {
    override def expectedMsg = "expected number or double string"
    override def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = s.toString
    override def visitString(s: CharSequence, index: Int) = s.toString
  }
  class NumReader[T](f: Long => T) extends Reader[T] {
    override def expectedMsg = "expected number"
    override def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      if (expIndex != -1) throw new AbortJsonProcessingException("expected integer")
      if (decIndex != -1) {
        var i = decIndex + 1
        while(i < s.length) {
          if (s.charAt(i) != '0') throw new AbortJsonProcessingException("expected integer")
          i += 1
        }
      }


      val end = if(decIndex != -1) decIndex else s.length
      var l = upickle.core.Util.parseLong(s, 0, end)
      if (expIndex == -1) f(l)
      else{
        val e = upickle.core.Util.parseLong(s, expIndex, s.length())
        var i = 0
        while(i < e){
          if (l >= Long.MaxValue / 10) throw new AbortJsonProcessingException("expected integer")
          l = l * 10
          i += 1
        }
        f(l)
      }
    }
  }
  implicit val DoubleReader: Reader[Double] = NumStringReader.map(_.toDouble)
  implicit val IntReader: Reader[Int] = new NumReader(l =>
    if (l > Int.MaxValue || l < Int.MinValue) throw new AbortJsonProcessingException("expected integer")
    else l.toInt
  )
  implicit val FloatReader: Reader[Float] = NumStringReader.map(_.toFloat)
  implicit val ShortReader: Reader[Short] = new NumReader(l =>
    if (l > Short.MaxValue || l < Short.MinValue) throw new AbortJsonProcessingException("expected short")
    else l.toShort
  )
  implicit val ByteReader: Reader[Byte] = new NumReader(l =>
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
  implicit val LongReader: Reader[Long] = new MapStringReader(s => core.Util.parseLong(s, 0, s.length()))
  implicit val BigIntReader: Reader[BigInt] = new MapStringReader(s => BigInt(s.toString))
  implicit val BigDecimalReader: Reader[BigDecimal] = new MapStringReader(s => BigDecimal(s.toString))
  implicit val SymbolReader: Reader[Symbol] = new MapStringReader(s => Symbol(s.toString))

  implicit def MapReader[K, V](implicit k: Reader[K], v: Reader[V]): Reader[Map[K, V]] = {
    if (k ne StringReader) SeqLikeReader[Array, (K, V)].map(_.toMap)
    else new Reader[Map[K, V]]{
      override def visitObject(index: Int) = new ObjVisitor[Any, Map[K, V]] {
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
    override def visitArray(index: Int) = new upickle.jawn.ArrVisitor[Any, C[T]] {
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
    }else Duration(core.Util.parseLong(s, 0, s.length()), TimeUnit.NANOSECONDS)
  )

  implicit val InfiniteDurationReader = DurationReader.narrow[Duration.Infinite]
  implicit val FiniteDurationReader = DurationReader.narrow[FiniteDuration]

  implicit def EitherReader[T1: Reader, T2: Reader] = new Reader[Either[T1, T2]]{
    override def expectedMsg = "expected sequence"
    override def visitArray(index: Int) = new upickle.jawn.ArrVisitor[Any, Either[T1, T2]] {
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

  implicit object JsValueR extends Reader[Js.Value]{
    override def visitObject(index: Int) = {
      new ObjVisitor[Any, Js.Obj] {
        val output = mutable.Buffer.empty[(String, Js.Value)]
        var lastKey: String = null
        def subVisitor = JsValueR

        def visitKey(s: CharSequence, index: Int): Unit = lastKey = s.toString

        def visitValue(v: Any, index: Int): Unit = {
          output.append((lastKey, v.asInstanceOf[Js.Value]))
        }

        def visitEnd(index: Int) = Js.Obj(output:_*)
      }
    }
    override def visitArray(index: Int) = {
      new ArrVisitor[Any, Js.Arr] {
        val output = mutable.Buffer.empty[Js.Value]
        def subVisitor = JsValueR

        def visitKey(s: CharSequence, index: Int): Unit = ???

        def visitValue(v: Any, index: Int): Unit = {
          output.append(v.asInstanceOf[Js.Value])
        }

        def visitEnd(index: Int) = Js.Arr(output:_*)
      }
    }
    override def visitString(s: CharSequence, index: Int) = Js.Str(s)
    override def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = Js.Num(s.toString.toDouble)
    override def visitTrue(index: Int) = Js.True
    override def visitFalse(index: Int) = Js.False
    override def visitNull(index: Int) = Js.Null
  }

  implicit def JsObjR: Reader[Js.Obj] = JsValueR.narrow[Js.Obj]


  implicit def JsArrR: Reader[Js.Arr] = JsValueR.narrow[Js.Arr]



  implicit def JsStrR: Reader[Js.Str] = JsValueR.narrow[Js.Str]


  implicit def JsNumR: Reader[Js.Num] = JsValueR.narrow[Js.Num]


  implicit def JsTrueR: Reader[Js.True.type] = JsValueR.narrow[Js.True.type]

  implicit def JsFalseR: Reader[Js.False.type] = JsValueR.narrow[Js.False.type]


  implicit def JsNullR: Reader[Js.Null.type] = JsValueR.narrow[Js.Null.type]

  implicit object IndexedJsValueR extends Reader[IndexedJs]{
    override def visitObject(index: Int) = {
      new ObjVisitor[Any, IndexedJs.Obj] {
        val output = mutable.Buffer.empty[(String, IndexedJs)]
        var lastKey: String = null
        def subVisitor = IndexedJsValueR

        def visitKey(s: CharSequence, index: Int): Unit = lastKey = s.toString

        def visitValue(v: Any, index: Int): Unit = {
          output.append((lastKey, v.asInstanceOf[IndexedJs]))
        }

        def visitEnd(index: Int) = IndexedJs.Obj(index, output:_*)

      }
    }
    override def visitArray(index: Int) = {
      new ArrVisitor[Any, IndexedJs.Arr] {
        val output = mutable.Buffer.empty[IndexedJs]
        def subVisitor = IndexedJsValueR

        def visitValue(v: Any, index: Int): Unit = {
          output.append(v.asInstanceOf[IndexedJs])
        }

        def visitEnd(index: Int) = IndexedJs.Arr(index, output:_*)
      }
    }
    override def visitString(s: CharSequence, index: Int) = IndexedJs.Str(index, s)
    override def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      IndexedJs.Num(index, s, decIndex, expIndex)
    }
    override def visitTrue(index: Int) = IndexedJs.True(index)
    override def visitFalse(index: Int) = IndexedJs.False(index)
    override def visitNull(index: Int) = IndexedJs.Null(index)
  }

  implicit def IndexedJsObjR: Reader[IndexedJs.Obj] = IndexedJsValueR.narrow[IndexedJs.Obj]
  implicit def IndexedJsArrR: Reader[IndexedJs.Arr] = IndexedJsValueR.narrow[IndexedJs.Arr]
  implicit def IndexedJsStrR: Reader[IndexedJs.Str] = IndexedJsValueR.narrow[IndexedJs.Str]
  implicit def IndexedJsNumR: Reader[IndexedJs.Num] = IndexedJsValueR.narrow[IndexedJs.Num]
  implicit def IndexedJsTrueR: Reader[IndexedJs.True] = IndexedJsValueR.narrow[IndexedJs.True]
  implicit def IndexedJsFalseR: Reader[IndexedJs.False] = IndexedJsValueR.narrow[IndexedJs.False]
  implicit def IndexedJsNullR: Reader[IndexedJs.Null] = IndexedJsValueR.narrow[IndexedJs.Null]
}