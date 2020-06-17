package upickle.implicits

import scala.quoted.{ given _, _ }
import deriving._, compiletime._

inline def getDefaultParams[T]: Map[String, AnyRef] = ${ getDefaultParmasImpl[T] }
def getDefaultParmasImpl[T](using qctx: QuoteContext,
  tpe: Type[T]): Expr[Map[String, AnyRef]] =
  import qctx.tasty._
  val sym = tpe.unseal.symbol
  val comp = sym.companionClass
  val names =
    for p <- sym.caseFields if p.flags.is(Flags.HasDefault)
    yield p.name
  val namesExpr: Expr[List[String]] =
    Expr.ofList(names.map(Expr(_)))

  val body = comp.tree.asInstanceOf[ClassDef].body
  val idents: List[Ref] =
    for case deff @ DefDef(name, _, _, _, tree) <- body
    if name.startsWith("$lessinit$greater$default")
    yield Ref(deff.symbol)
  val identsExpr: Expr[List[Any]] =
    Expr.ofList(idents.map(_.seal.cast[Any]))

  '{ $namesExpr.zip($identsExpr.map(_.asInstanceOf[AnyRef])).toMap }
end getDefaultParmasImpl

inline def summonAll[T <: Tuple]: List[_] =
  inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (t *: ts) => summonInline[t] :: summonAll[ts]
end summonAll

inline def constValueList[Types <: Tuple]: List[_] =
  inline erasedValue[Types] match
    case _: (t *: ts) => constValue[t] :: constValueList[ts]
    case _ => Nil
end constValueList

inline def isMemberOfSealedHierarchy[T]: Boolean = ${ isMemberOfSealedHierarchyImpl[T] }
def isMemberOfSealedHierarchyImpl[T](using qctx: QuoteContext,
  tpe: Type[T]): Expr[Boolean] =
  import qctx.tasty._

  val parents = tpe.unseal.tpe.baseClasses
  Expr(parents.exists { p => p.flags.is(Flags.Sealed) })


inline def fullClassName[T]: String = ${ fullClassNameImpl[T] }
def fullClassNameImpl[T](using qctx: QuoteContext,
  tpe: Type[T]): Expr[String] =
  import qctx.tasty._
  Expr(tpe.unseal.symbol.fullName)
