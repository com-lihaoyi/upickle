package upickle
package api

import java.util.UUID

import upickle.jawn.{ArrVisitor, ObjArrVisitor, ObjVisitor}

import scala.concurrent.duration.{Duration, FiniteDuration}

trait Writers extends upickle.core.Types with Generated with MacroImplicits{
  implicit object StringWriter extends Writer[String] {
    def write[R](out: upickle.jawn.Visitor[_, R], v: String): R = out.jstring(v)
  }
  implicit object UnitWriter extends Writer[Unit] {
    def write[R](out: upickle.jawn.Visitor[_, R], v: Unit): R = {
      if (v == null) out.jnull(-1)
      else out.objectContext(-1).finish(-1)
    }
  }

  object NumStringWriter extends Writer[String] {
    def write[R](out: upickle.jawn.Visitor[_, R], v: String): R = v match{
      case "-Infinity" | "Infinity" | "NaN" => out.jstring(v)
      case _ => out.jnum(v, -1, -1)
    }
  }
  implicit val DoubleWriter: Writer[Double] = NumStringWriter.comap[Double](_.toString)
  implicit val IntWriter: Writer[Int] = NumStringWriter.comap[Int](_.toString)
  implicit val FloatWriter: Writer[Float] = NumStringWriter.comap[Float](_.toString)
  implicit val ShortWriter: Writer[Short] = NumStringWriter.comap[Short](_.toString)
  implicit val ByteWriter: Writer[Byte] = NumStringWriter.comap[Byte](_.toString)

  implicit object BooleanWriter extends Writer[Boolean] {
    def write[R](out: upickle.jawn.Visitor[_, R], v: Boolean): R = {
      if(v) out.jtrue() else out.jfalse()
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
      if (v == null) out.jnull()
      else{
        val ctx = out.arrayContext().asInstanceOf[ObjArrVisitor[Any, R]]
        val x = v.iterator
        while(x.nonEmpty){
          val next = x.next().asInstanceOf[T]
          val written = r.write(out, next)
          ctx.add(written, -1)

        }
        ctx.finish(-1)
      }
    }
  }
  implicit def ArrayWriter[T](implicit r: Writer[T]) = new Writer[Array[T]] {
    def write[R](out: upickle.jawn.Visitor[_, R], v: Array[T]): R = {
      val ctx = out.arrayContext().asInstanceOf[ObjArrVisitor[Any, R]]
      var i = 0
      while(i < v.length){
        ctx.add(r.write(out, v(i)), -1)
        i += 1
      }

      ctx.finish(-1)
    }
  }

  implicit def MapWriter[K, V](implicit kw: Writer[K], vw: Writer[V]): Writer[Map[K, V]] = {
    if (kw eq StringWriter) new Writer[Map[String, V]]{
      def write[R](out: upickle.jawn.Visitor[_, R], v: Map[String, V]): R = {
        val ctx = out.objectContext().asInstanceOf[ObjVisitor[Any, R]]
        for(pair <- v){
          val (k1, v1) = pair
          ctx.visitKey(k1, -1)
          ctx.add(vw.write(out, v1), -1)

        }
        ctx.finish(-1)
      }
    }.asInstanceOf[Writer[Map[K, V]]]
    else SeqLikeWriter[Seq, (K, V)].comap[Map[K, V]](_.toSeq)
  }

  implicit object DurationWriter extends Writer[Duration]{
    def write[R](out: upickle.jawn.Visitor[_, R], v: Duration): R = v match{
      case Duration.Inf => out.jstring("inf", -1)
      case Duration.MinusInf => out.jstring("-inf", -1)
      case x if x eq Duration.Undefined => out.jstring("undef", -1)
      case _ => out.jstring(v.toNanos.toString, -1)
    }
  }

  implicit val InfiniteDurationWriter = DurationWriter.asInstanceOf[Writer[Duration.Infinite]]
  implicit val FiniteDurationWriter = DurationWriter.asInstanceOf[Writer[FiniteDuration]]

  implicit def EitherWriter[T1: Writer, T2: Writer] = new Writer[Either[T1, T2]]{
    def write[R](out: upickle.jawn.Visitor[_, R], v: Either[T1, T2]): R = v match{
      case Left(t1) =>
        val ctx = out.arrayContext().asInstanceOf[ObjArrVisitor[Any, R]]
        ctx.add(out.jnum("0", -1, -1), -1)

        ctx.add(implicitly[Writer[T1]].write(out, t1), -1)

        ctx.finish(-1)
      case Right(t2) =>
        val ctx = out.arrayContext().asInstanceOf[ObjArrVisitor[Any, R]]
        ctx.add(out.jnum("1", -1, -1), -1)

        ctx.add(implicitly[Writer[T2]].write(out, t2), -1)

        ctx.finish(-1)
    }
  }
  implicit def RightWriter[T1: Writer, T2: Writer] =
    EitherWriter[T1, T2].asInstanceOf[Writer[Right[T1, T2]]]
  implicit def LeftWriter[T1: Writer, T2: Writer] =
    EitherWriter[T1, T2].asInstanceOf[Writer[Left[T1, T2]]]

  implicit def JsObjW: Writer[Js.Obj] = JsValueW.asInstanceOf[Writer[Js.Obj]]
  implicit def JsArrW: Writer[Js.Arr] = JsValueW.asInstanceOf[Writer[Js.Arr]]
  implicit def JsStrW: Writer[Js.Str] = JsValueW.asInstanceOf[Writer[Js.Str]]
  implicit def JsNumW: Writer[Js.Num] = JsValueW.asInstanceOf[Writer[Js.Num]]
  implicit def JsTrueW: Writer[Js.True.type] = JsValueW.asInstanceOf[Writer[Js.True.type]]
  implicit def JsFalseW: Writer[Js.False.type] = JsValueW.asInstanceOf[Writer[Js.False.type]]
  implicit def JsNullW: Writer[Js.Null.type] = JsValueW.asInstanceOf[Writer[Js.Null.type]]
  implicit object JsValueW extends Writer[Js.Value] {
    def write[R](out: upickle.jawn.Visitor[_, R], v: Js.Value): R = {
      v match{
        case v: Js.Obj =>
          val ctx = out.objectContext(-1).asInstanceOf[ObjVisitor[Any, R]]
          for((k, item) <- v.value){

            ctx.visitKey(k, -1)
            ctx.add(JsValueW.write(out, item), -1)
          }

          ctx.finish(-1)
        case v: Js.Arr =>
          val ctx = out.arrayContext(-1).asInstanceOf[ArrVisitor[Any, R]]
          for(item <- v.value){
            ctx.add(JsValueW.write(out, item), -1)

          }

          ctx.finish(-1)
        case v: Js.Str => out.jstring(v.value, -1)
        case v: Js.Num => out.jnum(v.value.toString, -1, -1, -1)
        case v: Js.False.type => out.jfalse(-1)
        case v: Js.True.type => out.jtrue(-1)
        case v: Js.Null.type => out.jnull(-1)
      }
    }
  }
}