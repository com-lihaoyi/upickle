package upickle.implicits.macros

import scala.quoted.{ given, _ }
import deriving._, compiletime._

inline def getDefaultParams[T]: Map[String, AnyRef] = ${ getDefaultParmasImpl[T] }
def getDefaultParmasImpl[T](using Quotes, Type[T]): Expr[Map[String, AnyRef]] =
  import quotes.reflect._
  val sym = TypeTree.of[T].symbol

  if (sym.isClassDef) {
    val comp = if (sym.isClassDef) sym.companionClass else sym
    val hasDefaults =
      for p <- sym.caseFields
      yield p.flags.is(Flags.HasDefault)
    val names = fieldLabelsImpl0[T].zip(hasDefaults).collect{case (n, true) => n}
    val namesExpr: Expr[List[String]] =
      Expr.ofList(names.map(Expr(_)))

    val body = comp.tree.asInstanceOf[ClassDef].body
    val idents: List[Ref] =
      for case deff @ DefDef(name, _, _, _) <- body
      if name.startsWith("$lessinit$greater$default")
      yield Ref(deff.symbol)
    val identsExpr: Expr[List[Any]] =
      Expr.ofList(idents.map(_.asExpr))

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

def extractKey[A](using Quotes)(sym: quotes.reflect.Symbol): Option[String] =
  import quotes.reflect._
  sym
    .annotations
    .find(_.tpe =:= TypeRepr.of[upickle.implicits.key])
    .map{case Apply(_, Literal(StringConstant(s)) :: Nil) => s}
end extractKey

inline def fieldLabels[T] = ${fieldLabelsImpl[T]}
def fieldLabelsImpl0[T](using Quotes, Type[T]): List[String] =
  import quotes.reflect._
  val fields: List[Symbol] = TypeTree.of[T].symbol
    .primaryConstructor
    .paramSymss
    .flatten

  fields.map{ sym =>
    extractKey(sym) match {
      case Some(name) => name
      case None => sym.name
    }
  }
end fieldLabelsImpl0

def fieldLabelsImpl[T](using Quotes, Type[T]): Expr[List[String]] =
Expr.ofList(fieldLabelsImpl0[T].map(Expr(_)))
end fieldLabelsImpl


inline def isMemberOfSealedHierarchy[T]: Boolean = ${ isMemberOfSealedHierarchyImpl[T] }
def isMemberOfSealedHierarchyImpl[T](using Quotes, Type[T]): Expr[Boolean] =
  import quotes.reflect._

  val parents = TypeRepr.of[T].baseClasses
  Expr(parents.exists { p => p.flags.is(Flags.Sealed) })


inline def fullClassName[T]: String = ${ fullClassNameImpl[T] }
def fullClassNameImpl[T](using Quotes, Type[T]): Expr[String] =
  import quotes.reflect._

  val sym = TypeTree.of[T].symbol
  extractKey(sym) match {
    case Some(name) => Expr(name)
    case None => Expr(sym.fullName.replace("$", ""))
  }
end fullClassNameImpl
