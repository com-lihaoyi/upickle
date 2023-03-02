package upickle.implicits.macros

import scala.quoted.{ given, _ }
import deriving._, compiletime._

type IsInt[A <: Int] = A

def getDefaultParamsImpl0[T](using Quotes, Type[T]): Map[String, Expr[AnyRef]] =
  import quotes.reflect._
  val sym = TypeTree.of[T].symbol

  if (!sym.isClassDef) Map.empty
  else
    val comp =
      if (sym.isClassDef && !sym.companionClass.isNoSymbol ) sym.companionClass
      else sym

    val hasDefaults =
      for p <- sym.caseFields
      yield p.flags.is(Flags.HasDefault)

    val names = fieldLabelsImpl0[T].map(_._2).zip(hasDefaults).collect{case (n, true) => n}

    val body = comp.tree.asInstanceOf[ClassDef].body

    val idents: List[Ref] =
      for case deff @ DefDef(name, _, _, _) <- body
      if name.startsWith("$lessinit$greater$default")
      yield Ref(deff.symbol)

    names.zip(idents.map(_.asExpr).map(e => '{$e.asInstanceOf[AnyRef]})).toMap

inline def getDefaultParamsArray[T] = ${getDefaultParamsArray1[T]}
def getDefaultParamsArray1[T](using Quotes, Type[T]): Expr[Array[() => Any]] =
  '{${Expr.ofSeq(getDefaultParamsArray0[T])}.toArray}

def getDefaultParamsArray0[T](using Quotes, Type[T]): Seq[Expr[() => Any]] =
  val map = getDefaultParamsImpl0[T]
  fieldLabelsImpl0.map(_._2).map(map.get(_) match{
    case None => '{null}
    case Some(v) => '{() => $v}
  })

def extractKey[A](using Quotes)(sym: quotes.reflect.Symbol): Option[String] =
  import quotes.reflect._
  sym
    .annotations
    .find(_.tpe =:= TypeRepr.of[upickle.implicits.key])
    .map{case Apply(_, Literal(StringConstant(s)) :: Nil) => s}

inline def paramsCount[T]: Int = ${paramsCountImpl[T]}
def paramsCountImpl[T](using Quotes, Type[T]) = {
  Expr(fieldLabelsImpl0[T].size)
}

inline def storeDefaults[T](inline x: upickle.core.BaseCaseObjectContext): Unit = ${storeDefaultsImpl[T]('x)}
def storeDefaultsImpl[T](x: Expr[upickle.core.BaseCaseObjectContext])(using Quotes, Type[T]) = {
  import quotes.reflect.*

  val statements = fieldLabelsImpl0[T]
    .zipWithIndex
    .map { case ((rawLabel, label), i) =>
      val defaults = getDefaultParamsImpl0[T]
      if (defaults.contains(label)) '{${x}.storeValueIfNotFound(${Expr(i)}, ${defaults(label)})}
      else '{}
    }

  Expr.block(statements, '{})
}

inline def fieldLabels[T]: List[(String, String)] = ${fieldLabelsImpl[T]}
def fieldLabelsImpl0[T](using Quotes, Type[T]): List[(quotes.reflect.Symbol, String)] =
  import quotes.reflect._
  val fields: List[Symbol] = TypeRepr.of[T].typeSymbol
    .primaryConstructor
    .paramSymss
    .flatten
    .filterNot(_.isType)

  if (TypeRepr.of[T].isSingleton) Nil
  else fields.map{ sym =>
    extractKey(sym) match
    case Some(name) => (sym, name)
    case None => (sym, sym.name)
  }

def fieldLabelsImpl[T](using Quotes, Type[T]): Expr[List[(String, String)]] =
  Expr.ofList(fieldLabelsImpl0[T].map((a, b) => Expr((a.name, b))))

inline def keyToIndex[T](inline x: String): Int = ${keyToIndexImpl[T]('x)}
def keyToIndexImpl[T](x: Expr[String])(using Quotes, Type[T]): Expr[Int] = {
  import quotes.reflect.*
  val z = Match(
    x.asTerm,
    fieldLabelsImpl0[T].map(_._2).zipWithIndex.map{(f, i) =>
      CaseDef(Literal(StringConstant(f)), None, Literal(IntConstant(i)))
    } ++ Seq(
      CaseDef(Wildcard(), None, Literal(IntConstant(-1)))
    )
  )

  z.asExpr.asInstanceOf[Expr[Int]]
}

inline def writeLength[T](inline thisOuter: upickle.core.Types with upickle.implicits.MacrosCommon,
                          inline v: T): Int =
  ${writeLengthImpl[T]('thisOuter, 'v)}

def writeLengthImpl[T](thisOuter: Expr[upickle.core.Types with upickle.implicits.MacrosCommon],
                                       v: Expr[T])
                                      (using Quotes, Type[T]): Expr[Int] =
  import quotes.reflect.*
    fieldLabelsImpl0[T]
      .map{(rawLabel, label) =>
        val defaults = getDefaultParamsImpl0[T]
        val select = Select.unique(v.asTerm, rawLabel.name).asExprOf[Any]
        if (!defaults.contains(label)) '{1}
        else '{if (${thisOuter}.serializeDefaults || ${select} != ${defaults(label)}) 1 else 0}
      }
      .foldLeft('{0}) { case (prev, next) => '{$prev + $next} }

inline def checkErrorMissingKeysCount[T](): Long =
  ${checkErrorMissingKeysCountImpl[T]()}

def checkErrorMissingKeysCountImpl[T]()(using Quotes, Type[T]): Expr[Long] =
  import quotes.reflect.*
  val paramCount = fieldLabelsImpl0[T].size
  if (paramCount <= 64) if (paramCount == 64) Expr(-1) else Expr((1L << paramCount) - 1)
  else Expr(paramCount)

inline def writeSnippets[R, T, WS <: Tuple](inline thisOuter: upickle.core.Types with upickle.implicits.MacrosCommon,
                                   inline self: upickle.core.Types#CaseW[T],
                                   inline v: T,
                                   inline ctx: _root_.upickle.core.ObjVisitor[_, R]): Unit =
  ${writeSnippetsImpl[R, T, WS]('thisOuter, 'self, 'v, 'ctx)}

def writeSnippetsImpl[R, T, WS <: Tuple](thisOuter: Expr[upickle.core.Types with upickle.implicits.MacrosCommon],
                            self: Expr[upickle.core.Types#CaseW[T]],
                            v: Expr[T],
                            ctx: Expr[_root_.upickle.core.ObjVisitor[_, R]])
                           (using Quotes, Type[T], Type[R], Type[WS]): Expr[Unit] =

  import quotes.reflect.*

  Expr.block(
    for (((rawLabel, label), i) <- fieldLabelsImpl0[T].zipWithIndex) yield {

      val tpe0 = TypeRepr.of[T].memberType(rawLabel).asType
      tpe0 match
      case '[tpe] =>
        val defaults = getDefaultParamsImpl0[T]
        Literal(IntConstant(i)).tpe.asType match
        case '[IsInt[index]] =>
          val select = Select.unique(v.asTerm, rawLabel.name).asExprOf[Any]
          val snippet = '{
            ${self}.writeSnippet[R, tpe](
              ${thisOuter}.objectAttributeKeyWriteMap,
              ${ctx},
              ${Expr(label)},
              summonInline[Tuple.Elem[WS, index]],
              ${select},
            )
          }
          if (!defaults.contains(label)) snippet
          else '{if (${thisOuter}.serializeDefaults || ${select} != ${defaults(label)}) $snippet}

    },
    '{()}
  )

inline def isMemberOfSealedHierarchy[T]: Boolean = ${ isMemberOfSealedHierarchyImpl[T] }
def isMemberOfSealedHierarchyImpl[T](using Quotes, Type[T]): Expr[Boolean] =
  import quotes.reflect._

  val parents = TypeRepr.of[T].baseClasses

  Expr(parents.exists { p => p.flags.is(Flags.Sealed) })

inline def tagName[T]: String = ${ tagNameImpl[T] }
def tagNameImpl[T](using Quotes, Type[T]): Expr[String] =
  import quotes.reflect._

  val sym = TypeTree.of[T].symbol

  extractKey(sym) match
  case Some(name) => Expr(name)
  case None =>
    // In Scala 3 enums, we use the short name of each case as the tag, rather
    // than the fully-qualified name. We can do this because we know that all
    // enum cases are in the same `enum Foo` namespace with distinct short names,
    // whereas sealed trait instances could be all over the place with identical
    // short names only distinguishable by their prefix.
    //
    // Harmonizing these two cases further is TBD
    if (sym.flags.is(Flags.Enum)) Expr(sym.name.filter(_ != '$'))
    else Expr(TypeTree.of[T].tpe.typeSymbol.fullName.filter(_ != '$'))

inline def enumValueOf[T]: String => T = ${ enumValueOfImpl[T] }
def enumValueOfImpl[T](using Quotes, Type[T]): Expr[String => T] =
  import quotes.reflect._

  val sym = TypeTree.of[T].symbol
  val companion = sym.companionClass.tree.asInstanceOf[ClassDef]

  val valueOfMethod: DefDef = companion
    .body
    .collectFirst { case dd @ DefDef("valueOf", _, _, _) => dd }
    .getOrElse { throw Exception("Enumeration valueOf method not found") }

  val methodSymbol = valueOfMethod.symbol
  Ref(methodSymbol).etaExpand(methodSymbol.owner).asExpr.asInstanceOf[Expr[String => T]]

case class EnumDescription(name: String, values: Seq[String]):
  def pretty = s"$name[values: ${values.mkString(", ")}]"

inline def enumDescription[T](using m: Mirror.Of[T]): EnumDescription = inline m match
case m: Mirror.ProductOf[T] =>
  throw new UnsupportedOperationException("Products cannot have enum descriptions")

case m: Mirror.SumOf[T] =>
  val name = constValue[m.MirroredLabel]
  val values = constValueTuple[m.MirroredElemLabels].productIterator.toSeq.map(_.toString)
  EnumDescription(name, values)


inline def isSingleton[T]: Boolean = ${ isSingletonImpl[T] }
def isSingletonImpl[T](using Quotes, Type[T]): Expr[Boolean] =
  import quotes.reflect._
  Expr(TypeRepr.of[T].typeSymbol.flags.is(Flags.Module))

