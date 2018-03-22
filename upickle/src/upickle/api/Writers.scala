package upickle
package api

import java.util.UUID

import upickle.jawn.{ArrVisitor, ObjArrVisitor, ObjVisitor}

import scala.concurrent.duration.{Duration, FiniteDuration}

trait Writers extends upickle.core.Types with Generated with MacroImplicits{
  implicit object StringWriter extends Writer[String] {
    def write[R](out: upickle.jawn.Visitor[_, R], v: String): R = out.visitString(v)
  }
  implicit object UnitWriter extends Writer[Unit] {
    def write[R](out: upickle.jawn.Visitor[_, R], v: Unit): R = {
      if (v == null) out.visitNull(-1)
      else out.visitObject(-1).visitEnd(-1)
    }
  }

  object NumStringWriter extends Writer[String] {
    def write[R](out: upickle.jawn.Visitor[_, R], v: String): R = v match{
      case "-Infinity" | "Infinity" | "NaN" => out.visitString(v)
      case _ => out.visitNum(v, -1, -1)
    }
  }
  implicit val DoubleWriter: Writer[Double] = NumStringWriter.comap[Double](_.toString)
  implicit val IntWriter: Writer[Int] = NumStringWriter.comap[Int](_.toString)
  implicit val FloatWriter: Writer[Float] = NumStringWriter.comap[Float](_.toString)
  implicit val ShortWriter: Writer[Short] = NumStringWriter.comap[Short](_.toString)
  implicit val ByteWriter: Writer[Byte] = NumStringWriter.comap[Byte](_.toString)

  implicit object BooleanWriter extends Writer[Boolean] {
    def write[R](out: upickle.jawn.Visitor[_, R], v: Boolean): R = {
      if(v) out.visitTrue() else out.visitFalse()
    }
  }
  implicit val CharWriter: Writer[Char] = StringWriter.comap[Char](_.toString)
  implicit val UUIDWriter: Writer[UUID] = StringWriter.comap[UUID](_.toString)
  implicit val LongWriter: Writer[Long] = StringWriter.comap[Long](_.toString)
  implicit val BigIntWriter: Writer[BigInt] = StringWriter.comap[BigInt](_.toString)
  implicit val BigDecimalWriter: Writer[BigDecimal] = StringWriter.comap[BigDecimal](_.toString)
  implicit val SymbolWriter: Writer[Symbol] = StringWriter.comap[Symbol](_.name)

  implicit def OptionWriter[T: Writer]: Writer[Option[T]] = SeqLikeWriter[Seq, T].comap[Option[T]](_.toSeq)
  implicit def SomeWriter[T: Writer]: Writer[Some[T]] = OptionWriter[T].narrow[Some[T]]
  implicit def NoneWriter: Writer[None.type] = OptionWriter[Unit].narrow[None.type]
  implicit def SeqLikeWriter[C[_] <: Iterable[_], T](implicit r: Writer[T]) = new Writer[C[T]] {
    def write[R](out: upickle.jawn.Visitor[_, R], v: C[T]): R = {
      if (v == null) out.visitNull()
      else{
        val ctx = out.visitArray().narrow
        val x = v.iterator
        while(x.nonEmpty){
          val next = x.next().asInstanceOf[T]
          val written = r.write(ctx.subVisitor, next)
          ctx.visitValue(written, -1)
        }

        ctx.visitEnd(-1)
      }
    }
  }
  implicit def ArrayWriter[T](implicit r: Writer[T]) = new Writer[Array[T]] {
    def write[R](out: upickle.jawn.Visitor[_, R], v: Array[T]): R = {
      val ctx = out.visitArray().narrow
      var i = 0
      while(i < v.length){
        ctx.visitValue(r.write(ctx.subVisitor, v(i)), -1)
        i += 1
      }

      ctx.visitEnd(-1)
    }
  }

  implicit def MapWriter[K, V](implicit kw: Writer[K], vw: Writer[V]): Writer[Map[K, V]] = {
    if (kw eq StringWriter) new Writer[Map[String, V]]{
      def write[R](out: upickle.jawn.Visitor[_, R], v: Map[String, V]): R = {
        val ctx = out.visitObject().narrow
        for(pair <- v){
          val (k1, v1) = pair
          ctx.visitKey(k1, -1)
          ctx.visitValue(vw.write(ctx.subVisitor, v1), -1)

        }
        ctx.visitEnd(-1)
      }
    }.asInstanceOf[Writer[Map[K, V]]]
    else SeqLikeWriter[Seq, (K, V)].comap[Map[K, V]](_.toSeq)
  }

  implicit object DurationWriter extends Writer[Duration]{
    def write[R](out: upickle.jawn.Visitor[_, R], v: Duration): R = v match{
      case Duration.Inf => out.visitString("inf", -1)
      case Duration.MinusInf => out.visitString("-inf", -1)
      case x if x eq Duration.Undefined => out.visitString("undef", -1)
      case _ => out.visitString(v.toNanos.toString, -1)
    }
  }

  implicit val InfiniteDurationWriter = DurationWriter.narrow[Duration.Infinite]
  implicit val FiniteDurationWriter = DurationWriter.narrow[FiniteDuration]

  implicit def EitherWriter[T1: Writer, T2: Writer] = new Writer[Either[T1, T2]]{
    def write[R](out: upickle.jawn.Visitor[_, R], v: Either[T1, T2]): R = v match{
      case Left(t1) =>
        val ctx = out.visitArray().narrow
        ctx.visitValue(out.visitNum("0", -1, -1), -1)

        ctx.visitValue(implicitly[Writer[T1]].write(out, t1), -1)

        ctx.visitEnd(-1)
      case Right(t2) =>
        val ctx = out.visitArray().narrow
        ctx.visitValue(out.visitNum("1", -1, -1), -1)

        ctx.visitValue(implicitly[Writer[T2]].write(out, t2), -1)

        ctx.visitEnd(-1)
    }
  }
  implicit def RightWriter[T1: Writer, T2: Writer] =
    EitherWriter[T1, T2].narrow[Right[T1, T2]]
  implicit def LeftWriter[T1: Writer, T2: Writer] =
    EitherWriter[T1, T2].narrow[Left[T1, T2]]

  implicit def JsObjW: Writer[Js.Obj] = JsValueW.narrow[Js.Obj]
  implicit def JsArrW: Writer[Js.Arr] = JsValueW.narrow[Js.Arr]
  implicit def JsStrW: Writer[Js.Str] = JsValueW.narrow[Js.Str]
  implicit def JsNumW: Writer[Js.Num] = JsValueW.narrow[Js.Num]
  implicit def JsTrueW: Writer[Js.True.type] = JsValueW.narrow[Js.True.type]
  implicit def JsFalseW: Writer[Js.False.type] = JsValueW.narrow[Js.False.type]
  implicit def JsNullW: Writer[Js.Null.type] = JsValueW.narrow[Js.Null.type]
  implicit object JsValueW extends Writer[Js.Value] {
    def write[R](out: upickle.jawn.Visitor[_, R], v: Js.Value): R = {
      Js.transform(v, out)
    }
  }
}