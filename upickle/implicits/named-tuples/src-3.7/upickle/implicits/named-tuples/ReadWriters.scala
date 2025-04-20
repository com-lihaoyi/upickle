package upickle.implicits.namedTuples

import scala.NamedTuple.{AnyNamedTuple, Names, DropNames, NamedTuple, withNames}
import upickle.core.{Visitor, ObjVisitor}

import upickle.default.{Writer, Reader, ObjectWriter, CaseClassReader3V2}

object Macros:

  object Implicits:
    inline given [T <: AnyNamedTuple]: Writer[T] = Macros.derivedWriter[T]
    inline given [T <: AnyNamedTuple]: Reader[T] = Macros.derivedReader[T]

  inline def derivedWriter[T <: AnyNamedTuple]: Writer[T] =
    NTObjWriter[Names[T], DropNames[T]](
      fieldNames = compiletime.constValueTuple[Names[T]],
      fieldWriters = compiletime.summonAll[Tuple.Map[DropNames[T], Writer]]
    ).asInstanceOf[Writer[T]]

  inline def derivedReader[T <: AnyNamedTuple]: Reader[T] =
    NTObjReader[Names[T], DropNames[T]](
      paramCount = compiletime.constValue[NamedTuple.Size[T]],
      fieldNames = compiletime.constValueTuple[Names[T]],
      fieldReaders = compiletime.summonAll[Tuple.Map[DropNames[T], Reader]]
    ).asInstanceOf[Reader[T]]

  final class NTObjWriter[N <: Tuple, V <: Tuple](
      fieldNames: => Tuple,
      fieldWriters: => Tuple
  ) extends ObjectWriter[NamedTuple[N, V]]:
    private lazy val fW = fieldWriters
    private lazy val fN = fieldNames

    override def length(v: NamedTuple[N, V]): Int = fN.size

    override def writeToObject[R](
        ctx: ObjVisitor[?, R],
        v: NamedTuple[N, V]
    ): Unit =
      val iN = fN.productIterator.asInstanceOf[Iterator[String]]
      val iW = fW.productIterator.asInstanceOf[Iterator[Writer[Any]]]
      val iV = v.toTuple.productIterator.asInstanceOf[Iterator[Any]]
      iN.zip(iW).zip(iV).foreach { case ((n, w), v) =>
        val keyVisitor = ctx.visitKey(-1)
        ctx.visitKeyValue(
          keyVisitor.visitString(n, -1)
        )
        ctx.narrow.visitValue(w.write(ctx.subVisitor, v), -1)
      }

    override def write0[V1](out: Visitor[?, V1], v: NamedTuple[N, V]): V1 =
      val oVisitor = out.visitObject(fN.size, jsonableKeys = true, -1)
      writeToObject(oVisitor, v)
      oVisitor.visitEnd(-1)

  final class NTObjReader[N <: Tuple, V <: Tuple](
      paramCount: Int,
      fieldNames: => Tuple,
      fieldReaders: => Tuple
  ) extends CaseClassReader3V2[NamedTuple[N, V]](
        paramCount,
        if paramCount <= 64 then
          if paramCount == 64 then -1 else (1L << paramCount) - 1
        else paramCount,
        allowUnknownKeys = true,
        (params, _) => Tuple.fromArray(params).asInstanceOf[V].withNames[N]
      ):
    lazy val fR = fieldReaders.toArray
    lazy val fN = fieldNames.toArray.map(_.asInstanceOf[String])
    override def visitors0 = (null, fR)
    override def keyToIndex(x: String): Int = fN.indexOf(x)
    override def allKeysArray = fN
    override def storeDefaults(
        x: upickle.implicits.BaseCaseObjectContext
    ): Unit = ()
