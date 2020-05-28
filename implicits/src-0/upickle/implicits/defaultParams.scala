package upickle.implicits

import scala.quoted.{ given _, _ }

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
