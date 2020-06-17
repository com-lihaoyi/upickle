package upickle.implicits

import java.util.UUID

import upickle.core.{ Visitor, Annotator }

import scala.concurrent.duration.{Duration, FiniteDuration}

trait Writers extends upickle.core.Types
  with Generated
  with WritersVersionSpecific
  with LowPriWriters { this: Annotator =>
  implicit val StringWriter: Writer[String] = new Writer[String] {
    def write0[R](out: Visitor[_, R], v: String): R = out.visitString(v, -1)
  }
  implicit val UnitWriter: Writer[Unit] = new Writer[Unit] {
    def write0[R](out: Visitor[_, R], v: Unit): R = {
      out.visitObject(0, -1).visitEnd(-1)
    }
  }

  implicit val DoubleWriter: Writer[Double] = new Writer[Double] {
    def write0[R](out: Visitor[_, R], v: Double): R = out.visitFloat64(v, -1)
  }
  implicit val IntWriter: Writer[Int] = new Writer[Int] {
    def write0[V](out: Visitor[_, V], v: Int) = out.visitInt32(v, -1)
  }

  implicit val FloatWriter: Writer[Float] = new Writer[Float] {
    def write0[R](out: Visitor[_, R], v: Float): R = out.visitFloat32(v, -1)
  }
  implicit val ShortWriter: Writer[Short] = new Writer[Short] {
    def write0[V](out: Visitor[_, V], v: Short) = out.visitInt32(v, -1)
  }
  implicit val ByteWriter: Writer[Byte] = new Writer[Byte] {
    def write0[V](out: Visitor[_, V], v: Byte) = out.visitInt32(v, -1)
  }

  implicit val BooleanWriter: Writer[Boolean] = new Writer[Boolean] {
    def write0[R](out: Visitor[_, R], v: Boolean): R = {
      if(v) out.visitTrue(-1) else out.visitFalse(-1)
    }
  }
  implicit val CharWriter: Writer[Char] = new Writer[Char] {
    def write0[V](out: Visitor[_, V], v: Char) = out.visitChar(v, -1)
  }
  implicit val UUIDWriter: Writer[UUID] = StringWriter.comap[UUID](_.toString)
  implicit val LongWriter: Writer[Long] = new Writer[Long] {
    def write0[V](out: Visitor[_, V], v: Long) = out.visitInt64(v, -1)
  }
  implicit val BigIntWriter: Writer[BigInt] = StringWriter.comap[BigInt](_.toString)
  implicit val BigDecimalWriter: Writer[BigDecimal] = StringWriter.comap[BigDecimal](_.toString)
  implicit val SymbolWriter: Writer[Symbol] = StringWriter.comap[Symbol](_.name)

  implicit def OptionWriter[T: Writer]: Writer[Option[T]] = new Writer[Option[T]] {
    def write0[V](out: Visitor[_, V], v: Option[T]) = {
      val ctx = out.visitArray(v.size, -1).narrow
      val x = v.iterator
      v match{
        case None =>
        case Some(next) =>
        val written = implicitly[Writer[T]].write(ctx.subVisitor, next)
        ctx.visitValue(written, -1)
      }

      ctx.visitEnd(-1)
    }
  }

  implicit def SomeWriter[T: Writer]: Writer[Some[T]] = OptionWriter[T].narrow[Some[T]]
  implicit def NoneWriter: Writer[None.type] = OptionWriter[Unit].narrow[None.type]

  implicit def ArrayWriter[T](implicit r: Writer[T]): Writer[Array[T]] = {
    if (r == ByteWriter) new Writer[Array[T]] {
      def write0[R](out: Visitor[_, R], v: Array[T]): R = {
        out.visitBinary(v.asInstanceOf[Array[Byte]], 0, v.length, -1)
      }
    }
    else new Writer[Array[T]] {
      def write0[R](out: Visitor[_, R], v: Array[T]): R = {
        val ctx = out.visitArray(v.length, -1).narrow
        var i = 0
        while (i < v.length) {
          ctx.visitValue(r.write(ctx.subVisitor, v(i)), -1)
          i += 1
        }

        ctx.visitEnd(-1)
      }
    }
  }
  def MapWriter0[M[A, B] <: collection.Map[A, B], K, V]
                (implicit kw: Writer[K], vw: Writer[V]): Writer[M[K, V]] = {
    if (kw eq StringWriter) new Writer[M[String, V]]{
      def write0[R](out: Visitor[_, R], v: M[String, V]): R = {
        val ctx = out.visitObject(v.size, -1).narrow
        for(pair <- v){
          val (k1, v1) = pair
          val keyVisitor = ctx.visitKey(-1)
          ctx.visitKeyValue(keyVisitor.visitString(k1, -1))
          ctx.visitValue(vw.write(ctx.subVisitor, v1), -1)

        }
        ctx.visitEnd(-1)
      }
    }.asInstanceOf[Writer[M[K, V]]]
    else SeqLikeWriter[Seq, (K, V)].comap[M[K, V]](_.toSeq)
  }
  implicit def MapWriter1[K, V](implicit kw: Writer[K], vw: Writer[V]): Writer[collection.Map[K, V]] = {
    MapWriter0[collection.Map, K, V]
  }
  implicit def MapWriter2[K, V](implicit kw: Writer[K], vw: Writer[V]): Writer[collection.immutable.Map[K, V]] = {
    MapWriter0[collection.immutable.Map, K, V]
  }
  implicit def MapWriter3[K, V](implicit kw: Writer[K], vw: Writer[V]): Writer[collection.mutable.Map[K, V]] = {
    MapWriter0[collection.mutable.Map, K, V]
  }

  implicit val DurationWriter: Writer[Duration] = new Writer[Duration]{
    def write0[R](out: Visitor[_, R], v: Duration): R = v match{
      case Duration.Inf => out.visitString("inf", -1)
      case Duration.MinusInf => out.visitString("-inf", -1)
      case x if x eq Duration.Undefined => out.visitString("undef", -1)
      case _ => out.visitString(v.toNanos.toString, -1)
    }
  }

  implicit val InfiniteDurationWriter: Writer[Duration.Infinite] = DurationWriter.narrow[Duration.Infinite]
  implicit val FiniteDurationWriter: Writer[FiniteDuration] = DurationWriter.narrow[FiniteDuration]

  implicit def EitherWriter[T1: Writer, T2: Writer]: Writer[Either[T1, T2]] = new Writer[Either[T1, T2]]{
    def write0[R](out: Visitor[_, R], v: Either[T1, T2]): R = v match{
      case Left(t1) =>
        val ctx = out.visitArray(2, -1).narrow
        ctx.visitValue(ctx.subVisitor.visitFloat64StringParts("0", -1, -1, -1), -1)

        ctx.visitValue(implicitly[Writer[T1]].write(ctx.subVisitor, t1), -1)

        ctx.visitEnd(-1)
      case Right(t2) =>
        val ctx = out.visitArray(2, -1).narrow
        ctx.visitValue(ctx.subVisitor.visitFloat64StringParts("1", -1, -1, -1), -1)

        ctx.visitValue(implicitly[Writer[T2]].write(ctx.subVisitor, t2), -1)

        ctx.visitEnd(-1)
    }
  }
  implicit def RightWriter[T1: Writer, T2: Writer]: Writer[Right[T1, T2]] =
    EitherWriter[T1, T2].narrow[Right[T1, T2]]
  implicit def LeftWriter[T1: Writer, T2: Writer]: Writer[Left[T1, T2]] =
    EitherWriter[T1, T2].narrow[Left[T1, T2]]
}

/**
  * This needs to be split into a separate trait due to https://github.com/scala/bug/issues/11768
  */
trait LowPriWriters extends upickle.core.Types{
  implicit def SeqLikeWriter[C[_] <: Iterable[_], T](implicit r: Writer[T]): Writer[C[T]] = new Writer[C[T]] {
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
}
