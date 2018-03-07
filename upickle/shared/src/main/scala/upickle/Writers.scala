package upickle

import java.util.UUID

import jawn.RawFContext

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.concurrent.duration.{Duration, FiniteDuration}

trait Writers extends Types{
  implicit object StringWriter extends Writer[String] {
    def write(out: jawn.Facade[Unit], v: String) = out.jstring(v)
  }
  implicit object UnitWriter extends Writer[Unit] {
    def write(out: jawn.Facade[Unit], v: Unit) = out.jnull()
  }

  object NumStringWriter extends Writer[String] {
    def write(out: jawn.Facade[Unit], v: String) = out.jnum(v, -1, -1)
  }
  implicit val DoubleWriter: Writer[Double] = NumStringWriter.comap[Double](_.toString)
  implicit val IntWriter: Writer[Int] = NumStringWriter.comap[Int](_.toString)
  implicit val FloatWriter: Writer[Float] = NumStringWriter.comap[Float](_.toString)
  implicit val ShortWriter: Writer[Short] = NumStringWriter.comap[Short](_.toString)
  implicit val ByteWriter: Writer[Byte] = NumStringWriter.comap[Byte](_.toString)

  implicit object BooleanWriter extends Writer[Boolean] {
    def write(out: jawn.Facade[Unit], v: Boolean) = {
      if(v) out.jtrue() else out.jfalse()
    }
  }
  implicit val CharWriter: Writer[Char] = StringWriter.comap[Char](_.toString)
  implicit val UUIDWriter: Writer[UUID] = StringWriter.comap[UUID](_.toString)
  implicit val LongWriter: Writer[Long] = StringWriter.comap[Long](_.toString)
  implicit val BigIntWriter: Writer[BigInt] = StringWriter.comap[BigInt](_.toString)
  implicit val BigDecimalWriter: Writer[BigDecimal] = StringWriter.comap[BigDecimal](_.toString)
  implicit val SymbolWriter: Writer[Symbol] = StringWriter.comap[Symbol]{x =>
    println("Y: " + x.name)
    x.name
  }

  implicit def OptionWriter[T: Writer]: Writer[Option[T]] = SeqLikeWriter[Seq, T].comap[Option[T]](_.toSeq)
  implicit def SomeWriter[T: Writer]: Writer[Some[T]] = SeqLikeWriter[Seq, T].comap[Some[T]](_.toSeq)
  implicit def NoneWriter[T: Writer]: Writer[None.type] = SeqLikeWriter[Seq, T].comap[None.type](_.toSeq)
  implicit def SeqLikeWriter[C[_] <: Iterable[_], T](implicit r: Writer[T]) = new Writer[C[T]] {
    def write(out: jawn.Facade[Unit], v: C[T]) = {
      val ctx = out.arrayContext().asInstanceOf[RawFContext[Unit, Unit]]
      val x = v.iterator
      while(x.nonEmpty){
        ctx.add((), -1)
        r.write(out, x.next().asInstanceOf[T])
      }
      ctx.finish(-1)
    }
  }
  implicit def ArrayWriter[T](implicit r: Writer[T]) = new Writer[Array[T]] {
    def write(out: jawn.Facade[Unit], v: Array[T]) = {
      val ctx = out.arrayContext().asInstanceOf[RawFContext[Unit, Unit]]
      for(item <- v){
        ctx.add((), -1)
        r.write(out, item)
      }
      ctx.finish(-1)
    }
  }

  implicit def MapWriter[K, V](implicit kw: Writer[K], vw: Writer[V]): Writer[Map[K, V]] = {
    if (kw eq StringWriter) new Writer[Map[String, V]]{
      def write(out: jawn.Facade[Unit], v: Map[String, V]) = {
        val ctx = out.objectContext().asInstanceOf[RawFContext[Unit, Unit]]
        for((k1, v1) <- v){

          ctx.add(k1, -1)

          ctx.add(vw.write(out, v1), -1)

        }
        ctx.finish(-1)
      }
    }.asInstanceOf[Writer[Map[K, V]]]
    else SeqLikeWriter[Seq, (K, V)].comap[Map[K, V]](_.toSeq)
  }

  def TupleNWriter[V](writers: List[Writer[_]], f: V => Seq[Any]) = new Writer[V]{
    def write(out: jawn.Facade[Unit], v: V) = {
      val ctx = out.arrayContext()
      var first = true

      for((item, w) <- f(v).zip(writers)){
        if (!first) ctx.asInstanceOf[jawn.FContext[Unit]].add(null, -1)
        first = false
        w.asInstanceOf[Writer[Any]].write(out, item)
      }
      ctx.finish(-1)
    }
  }

  implicit def Tuple1Writer[T1](implicit t1: Writer[T1]): Writer[Tuple1[T1]] =
    TupleNWriter[Tuple1[T1]](List(t1), _.productIterator.toSeq)

  implicit def Tuple2Writer[T1, T2](implicit t1: Writer[T2], t2: Writer[T2]): Writer[Tuple2[T1, T2]] =
    TupleNWriter[Tuple2[T1, T2]](List(t1, t2), _.productIterator.toSeq)

  implicit object DurationWriter extends Writer[Duration]{
    def write(out: jawn.Facade[Unit], v: Duration) = v match{
      case Duration.Inf => out.jstring("inf", -1)
      case Duration.MinusInf => out.jstring("-inf", -1)
      case Duration.Undefined => out.jstring("undef", -1)
      case _ => out.jstring(v.toNanos.toString, -1)
    }
  }

  implicit val InfiniteDurationWriter = DurationWriter.asInstanceOf[Writer[Duration.Infinite]]
  implicit val FiniteDurationWriter = DurationWriter.asInstanceOf[Writer[FiniteDuration]]

  implicit def EitherWriter[T1: Writer, T2: Writer] = new Writer[Either[T1, T2]]{
    def write(out: jawn.Facade[Unit], v: Either[T1, T2]) = v match{
      case Left(t1) =>
        val ctx = out.arrayContext().asInstanceOf[RawFContext[Unit, Unit]]
        ctx.add((), -1)
        out.jnum("0", -1, -1)
        ctx.add((), -1)
        implicitly[Writer[T1]].write(out, t1)
        ctx.finish(-1)
      case Right(t2) =>
        val ctx = out.arrayContext().asInstanceOf[RawFContext[Unit, Unit]]
        ctx.add((), -1)
        out.jnum("1", -1, -1)
        ctx.add((), -1)
        implicitly[Writer[T2]].write(out, t2)
        ctx.finish(-1)
    }
  }
  implicit def RightWriter[T1: Writer, T2: Writer] =
    EitherWriter[T1, T2].asInstanceOf[Writer[Right[T1, T2]]]
  implicit def LeftWriter[T1: Writer, T2: Writer] =
    EitherWriter[T1, T2].asInstanceOf[Writer[Left[T1, T2]]]
}