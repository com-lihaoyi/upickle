package upickle.implicits.macros

import scala.quoted.{ given, _ }
import deriving._, compiletime._

inline def getDefaultParams[T]: Map[String, AnyRef] = ${ getDefaultParamsImpl[T] }
def getDefaultParamsImpl[T](using Quotes, Type[T]): Expr[Map[String, AnyRef]] =
  import quotes.reflect._
  val sym = TypeTree.of[T].symbol

  if (sym.isClassDef) {
    val comp =
      if (sym.isClassDef && !sym.companionClass.isNoSymbol ) sym.companionClass
      else sym

    val hasDefaults =
      for p <- sym.caseFields
      yield p.flags.is(Flags.HasDefault)
    val names = fieldLabelsImpl0[T].map(_._2).zip(hasDefaults).collect{case (n, true) => n}
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
end getDefaultParamsImpl

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

inline def fieldLabels[T]: List[(String, String)] = ${fieldLabelsImpl[T]}
def fieldLabelsImpl0[T](using Quotes, Type[T]): List[(quotes.reflect.Symbol, String)] =
  import quotes.reflect._
  val fields: List[Symbol] = TypeRepr.of[T].typeSymbol
    .primaryConstructor
    .paramSymss
    .flatten
    .filterNot(_.isType)

  fields.map{ sym =>
    extractKey(sym) match {
      case Some(name) => (sym, name)
      case None => (sym, sym.name)
    }
  }
end fieldLabelsImpl0

def fieldLabelsImpl[T](using Quotes, Type[T]): Expr[List[(String, String)]] =
  Expr.ofList(fieldLabelsImpl0[T].map((a, b) => Expr((a.name, b))))

inline def writeSnippets[R, T, WS <: Tuple](inline thisOuter: upickle.core.Types with upickle.implicits.MacrosCommon,
                                   inline self: upickle.core.Types#CaseW[T],
                                   inline v: T,
                                   inline ctx: _root_.upickle.core.ObjVisitor[_, R],
                                   inline writers: List[Any]): Unit =
  ${writeSnippetsImpl[R, T, WS]('thisOuter, 'self, 'v, 'ctx, 'writers)}

def writeSnippetsImpl[R, T, WS <: Tuple](thisOuter: Expr[upickle.core.Types with upickle.implicits.MacrosCommon],
                            self: Expr[upickle.core.Types#CaseW[T]],
                            v: Expr[T],
                            ctx: Expr[_root_.upickle.core.ObjVisitor[_, R]],
                            writers: Expr[List[Any]])
                           (using Quotes, Type[T], Type[R], Type[WS]): Expr[Unit] =

  import quotes.reflect.*

//  println("TypeRepr.of[WS] " + TypeRepr.of[WS])
//  println("TypeRepr.of[WS].asInstanceOf[AppliedType].args(1) " + TypeRepr.of[WS].asInstanceOf[AppliedType].args(1))

  Expr.block(
    for (((rawLabel, label), i) <- fieldLabelsImpl0[T].zipWithIndex) yield {

      val tpe0 = TypeRepr.of[T].memberType(rawLabel).asType
      tpe0 match{
        case '[tpe] =>
//          val writerType = Applied(TypeSelect(thisOuter.asTerm, "Writer"), List(TypeTree.of[tpe]))
            Literal(IntConstant(i)).tpe.asType match{
            case '[index] =>
              '{
                ${self}.writeSnippet[R, tpe](
                  ${thisOuter}.objectAttributeKeyWriteMap,
                  ${ctx},
                  ${Expr(label)},
                  ${writers}(${Expr(i)}),
//                  summonInline[Tuple.Elem[WS, index]],
                  ${Select.unique(v.asTerm, rawLabel.name).asExprOf[Any]}
                )
              }
          }
      }
    },
    '{()}
  )


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

inline def enumValueOf[T]: String => T = ${ enumValueOfImpl[T] }
def enumValueOfImpl[T](using Quotes, Type[T]): Expr[String => T] =
  import quotes.reflect._

  val sym = TypeTree.of[T].symbol
  val companion = sym.companionClass.tree.asInstanceOf[ClassDef]

  val valueOfMethod: DefDef = companion.body.collectFirst {
    case dd @ DefDef("valueOf", _, _, _) => dd
  }.getOrElse {
    throw Exception("Enumeration valueOf method not found")
  }

  val methodSymbol = valueOfMethod.symbol
  Ref(methodSymbol).etaExpand(methodSymbol.owner).asExpr.asInstanceOf[Expr[String => T]]
end enumValueOfImpl

case class EnumDescription(name: String, values: Seq[String]) {
  def pretty = s"$name[values: ${values.mkString(", ")}]"
}

inline def enumDescription[T](using m: Mirror.Of[T]): EnumDescription = inline m match {
  case m: Mirror.ProductOf[T] =>
    throw new UnsupportedOperationException("Products cannot have enum descriptions")

  case m: Mirror.SumOf[T] =>
    val name = constValue[m.MirroredLabel]
    val values = constValueTuple[m.MirroredElemLabels].productIterator.toSeq.map(_.toString)
    EnumDescription(name, values)
}


inline def isSingleton[T]: Boolean = ${ isSingletonImpl[T] }
def isSingletonImpl[T](using Quotes, Type[T]): Expr[Boolean] =
  import quotes.reflect._
  Expr(TypeRepr.of[T].isSingleton)

