package upickle.implicits.namedTuples

import scala.NamedTuple.{AnyNamedTuple, Names, DropNames, NamedTuple, withNames}
import upickle.core.{Visitor, ObjVisitor}

object default extends NamedTupleReadWriters[upickle.default.type] {
  val outer = upickle.default
}
object legacy extends NamedTupleReadWriters[upickle.legacy.type] {
  val outer = upickle.legacy
}

trait NamedTupleReadWriters[
    Impl <: upickle.core.Types & upickle.implicits.ReadersVersionSpecific
] {
  val outer: Impl

  object strict {
    final inline given givenDerivedNamedTupleWriter[T <: AnyNamedTuple]
        : outer.Writer[T] = derivedWriter[T]
    final inline given givenDerivedNamedTupleReader[T <: AnyNamedTuple]
        : outer.Reader[T] = derivedReader[T](strict = true)
  }

  final inline given givenDerivedNamedTupleWriter[T <: AnyNamedTuple]
      : outer.Writer[T] = derivedWriter[T]
  final inline given givenDerivedNamedTupleReader[T <: AnyNamedTuple]
      : outer.Reader[T] = derivedReader[T](strict = false)

  final inline def derivedWriter[T <: AnyNamedTuple]: outer.Writer[T] =
    writerNamedTuple[T, Names[T], DropNames[T]](
      fieldNames = compiletime.constValueTuple[Names[T]],
      fieldWriters =
        () => compiletime.summonAll[Tuple.Map[DropNames[T], outer.Writer]]
    )

  final inline def derivedReader[T <: AnyNamedTuple](strict: Boolean): outer.Reader[T] =
    readerNamedTuple[T, Names[T], DropNames[T]](
      paramCount = compiletime.constValue[NamedTuple.Size[T]],
      fieldNames = compiletime.constValueTuple[Names[T]],
      fieldReaders =
        () => compiletime.summonAll[Tuple.Map[DropNames[T], outer.Reader]],
      strict
    )

  final def writerNamedTuple[T <: AnyNamedTuple, N <: Tuple, V <: Tuple](
      fieldNames: N,
      fieldWriters: () => Tuple.Map[V, outer.Writer]
  ): outer.Writer[T] =
    NTObjWriter[N, V](fieldNames, fieldWriters).asInstanceOf[outer.Writer[T]]

  final def readerNamedTuple[T <: AnyNamedTuple, N <: Tuple, V <: Tuple](
      paramCount: Int,
      fieldNames: N,
      fieldReaders: () => Tuple.Map[V, outer.Reader],
      strict: Boolean
  ): outer.Reader[T] = NTObjReader[N, V](paramCount, fieldNames, fieldReaders, strict)
    .asInstanceOf[outer.Reader[T]]

  private def tupleIterator[T](t: Tuple): Iterator[T] =
    t.productIterator.asInstanceOf[Iterator[T]]

  private final class NTObjWriter[N <: Tuple, V <: Tuple](
      fieldNames: N,
      fieldWriters: () => Tuple.Map[V, outer.Writer]
  ) extends outer.ObjectWriter[NamedTuple[N, V]] {
    private val fN =
      tupleIterator[String](fieldNames).toArray
    private lazy val fW =
      tupleIterator[outer.Writer[Any]](fieldWriters()).toArray

    override def length(v: NamedTuple[N, V]): Int = fN.length

    override def writeToObject[R](
        ctx: ObjVisitor[?, R],
        v: NamedTuple[N, V]
    ): Unit = {
      val iN = fN.iterator
      val iW = fW.iterator
      val iV = tupleIterator[Any](v)

      while iN.hasNext && iW.hasNext && iV.hasNext do {
        val n = iN.next()
        val w = iW.next()
        val v = iV.next()

        val keyVisitor = ctx.visitKey(-1)
        ctx.visitKeyValue(
          keyVisitor.visitString(n, -1)
        )
        ctx.narrow.visitValue(w.write(ctx.subVisitor, v), -1)
      }
    }

    override def write0[V1](out: Visitor[?, V1], v: NamedTuple[N, V]): V1 = {
      val oVisitor = out.visitObject(fN.length, jsonableKeys = true, -1)
      writeToObject(oVisitor, v)
      oVisitor.visitEnd(-1)
    }
  }

  private def tupleFromArray[V <: Tuple](arr: Array[Any]): V =
    Tuple.fromArray(arr).asInstanceOf[V]

  private final class NTObjReader[N <: Tuple, V <: Tuple](
      paramCount: Int,
      fieldNames: N,
      fieldReaders: () => Tuple.Map[V, outer.Reader],
      strict: Boolean
  ) extends outer.CaseClassReader3V2[NamedTuple[N, V]](
        paramCount,
        if paramCount <= 64 then
          if paramCount == 64 then -1 else (1L << paramCount) - 1
        else paramCount,
        allowUnknownKeys = !strict,
        (params, _) => tupleFromArray[V](params).withNames[N]
      ) {
    private lazy val fR: Array[AnyRef] = fieldReaders().toArray
    private val fN: Array[String] =
      Array.from(tupleIterator[String](fieldNames))
    private val fN_Map: scala.collection.mutable.Map[String, Int] =
      fN.zipWithIndex.to(scala.collection.mutable.HashMap).withDefaultValue(-1)

    override def visitors0: (AnyRef, Array[AnyRef]) = (null, fR)

    override def keyToIndex(x: String): Int = fN_Map(x)

    override def allKeysArray: Array[String] = fN

    override def storeDefaults(
        x: upickle.implicits.BaseCaseObjectContext
    ): Unit = ()
  }
}
