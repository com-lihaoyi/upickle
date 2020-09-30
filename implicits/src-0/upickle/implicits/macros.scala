package upickle.implicits.macros

import scala.quoted.{ given _, _ }
import deriving._, compiletime._

inline def getDefaultParams[T]: Map[String, AnyRef] = ${ getDefaultParmasImpl[T] }
def getDefaultParmasImpl[T](using qctx: QuoteContext,
  tpe: Type[T]): Expr[Map[String, AnyRef]] =
  import qctx.tasty._
  val sym = tpe.unseal.symbol

  if (sym.isClassDef) {
    val comp = if (sym.isClassDef) sym.companionClass else sym
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
  } else {
    '{ Map.empty }
  }
end getDefaultParmasImpl

inline def summonList[T <: Tuple]: List[_] =
  inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (t *: ts) => summonInline[t] :: summonList[ts]
end summonList

def extractKey[A](using qctx: QuoteContext)(sym: qctx.tasty.Symbol): Option[String] =
  import qctx.tasty._
  sym
    .annots
    .find(_.tpe =:= Type.of[upickle.implicits.key])
    .map{case Apply(_, Literal(Constant.String(s)) :: Nil) => s}
end extractKey

inline def fieldLabels[T] = ${fieldLabelsImpl[T]}
def fieldLabelsImpl[T](using qctx: QuoteContext, tpe: Type[T]): Expr[List[String]] =
  import qctx.tasty._
  val fields: List[Symbol] = tpe.unseal.symbol
    .primaryConstructor
    .paramSymss
    .flatten

  val names = fields.map{ sym =>
    extractKey(sym) match {
      case Some(name) => name
      case None => sym.name
    }
  }
  Expr.ofList(names.map(Expr(_)))
end fieldLabelsImpl

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

  val sym = tpe.unseal.symbol
  extractKey(sym) match {
    case Some(name) => Expr(name)
    case None => Expr(sym.fullName.replace("$", ""))
  }
end fullClassNameImpl
