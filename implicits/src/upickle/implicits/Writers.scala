package upickle.implicits

import java.util.UUID

import upickle.core.Visitor

import scala.concurrent.duration.{Duration, FiniteDuration}

trait Writers extends upickle.core.Types with Generated with MacroImplicits{
  implicit object StringWriter extends Writer[String] {
    def write0[R](out: Visitor[_, R], v: String): R = out.visitString(v, -1)
  }
  implicit object UnitWriter extends Writer[Unit] {
    def write0[R](out: Visitor[_, R], v: Unit): R = {
      out.visitObject(0, -1).visitEnd(-1)
    }
  }

  implicit object DoubleWriter extends Writer[Double] {
    def write0[R](out: Visitor[_, R], v: Double): R = out.visitFloat64(v, -1)
  }
  implicit object IntWriter extends Writer[Int] {
    def write0[V](out: Visitor[_, V], v: Int) = out.visitInt32(v, -1)
  }

  implicit object FloatWriter extends Writer[Float] {
    def write0[R](out: Visitor[_, R], v: Float): R = out.visitFloat32(v, -1)
  }
  implicit object ShortWriter extends Writer[Short] {
    def write0[V](out: Visitor[_, V], v: Short) = out.visitInt32(v, -1)
  }
  implicit object ByteWriter extends Writer[Byte] {
    def write0[V](out: Visitor[_, V], v: Byte) = out.visitInt32(v, -1)
  }

  implicit object BooleanWriter extends Writer[Boolean] {
    def write0[R](out: Visitor[_, R], v: Boolean): R = {
      if(v) out.visitTrue(-1) else out.visitFalse(-1)
    }
  }
  implicit object CharWriter extends Writer[Char] {
    def write0[V](out: Visitor[_, V], v: Char) = out.visitChar(v, -1)
  }
  implicit val UUIDWriter: Writer[UUID] = StringWriter.comap[UUID](_.toString)
  implicit object LongWriter extends Writer[Long] {
    def write0[V](out: Visitor[_, V], v: Long) = out.visitInt64(v, -1)
  }
  implicit val BigIntWriter: Writer[BigInt] = StringWriter.comap[BigInt](_.toString)
  implicit val BigDecimalWriter: Writer[BigDecimal] = StringWriter.comap[BigDecimal](_.toString)
  implicit val SymbolWriter: Writer[Symbol] = StringWriter.comap[Symbol](_.name)

  implicit def OptionWriter[T: Writer]: Writer[Option[T]] = SeqLikeWriter[Seq, T].comap[Option[T]](_.toSeq)
  implicit def SomeWriter[T: Writer]: Writer[Some[T]] = OptionWriter[T].narrow[Some[T]]
  implicit def NoneWriter: Writer[None.type] = OptionWriter[Unit].narrow[None.type]
  implicit def SeqLikeWriter[C[_] <: Iterable[_], T](implicit r: Writer[T]) = new Writer[C[T]] {
    def write0[R](out: Visitor[_, R], v: C[T]): R = {
      val ctx = out.visitArray(v.size, -1).narrow
      val x = v.iterator
      while(x.nonEmpty){
        val next = x.next().asInstanceOf[T]
        val written = r.write(ctx.subVisitor, next)
        ctx.visitValue(written, -1)
      }

      ctx.visitEnd(-1)
    }
  }
  implicit def ArrayWriter[T](implicit r: Writer[T]) = new Writer[Array[T]] {
    def write0[R](out: Visitor[_, R], v: Array[T]): R = {
      val ctx = out.visitArray(v.length, -1).narrow
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
      def write0[R](out: Visitor[_, R], v: Map[String, V]): R = {
        val ctx = out.visitObject(v.size, -1).narrow
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
    def write0[R](out: Visitor[_, R], v: Duration): R = v match{
      case Duration.Inf => out.visitString("inf", -1)
      case Duration.MinusInf => out.visitString("-inf", -1)
      case x if x eq Duration.Undefined => out.visitString("undef", -1)
      case _ => out.visitString(v.toNanos.toString, -1)
    }
  }

  implicit val InfiniteDurationWriter = DurationWriter.narrow[Duration.Infinite]
  implicit val FiniteDurationWriter = DurationWriter.narrow[FiniteDuration]

  implicit def EitherWriter[T1: Writer, T2: Writer] = new Writer[Either[T1, T2]]{
    def write0[R](out: Visitor[_, R], v: Either[T1, T2]): R = v match{
      case Left(t1) =>
        val ctx = out.visitArray(2, -1).narrow
        ctx.visitValue(out.visitFloat64StringParts("0", -1, -1, -1), -1)

        ctx.visitValue(implicitly[Writer[T1]].write(ctx.subVisitor, t1), -1)

        ctx.visitEnd(-1)
      case Right(t2) =>
        val ctx = out.visitArray(2, -1).narrow
        ctx.visitValue(out.visitFloat64StringParts("1", -1, -1, -1), -1)

        ctx.visitValue(implicitly[Writer[T2]].write(ctx.subVisitor, t2), -1)

        ctx.visitEnd(-1)
    }
  }
  implicit def RightWriter[T1: Writer, T2: Writer] =
    EitherWriter[T1, T2].narrow[Right[T1, T2]]
  implicit def LeftWriter[T1: Writer, T2: Writer] =
    EitherWriter[T1, T2].narrow[Left[T1, T2]]
}