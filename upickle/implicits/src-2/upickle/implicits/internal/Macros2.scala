package upickle.implicits.internal

import scala.annotation.{nowarn, StaticAnnotation}
import scala.language.experimental.macros
import compat._

import acyclic.file
import upickle.core.Annotator
import upickle.implicits.{MacrosCommon, flatten, key}
import language.higherKinds
import language.existentials

/**
 * Implementation of macros used by uPickle to serialize and deserialize
 * case classes automatically. You probably shouldn't need to use these
 * directly, since they are called implicitly when trying to read/write
 * types you don't have a Reader/Writer in scope for.
 */
@nowarn("cat=deprecation")
object Macros2 {

  private[upickle] trait DeriveDefaults[M[_]] {
    val c: scala.reflect.macros.blackbox.Context
    private[upickle] def fail(s: String) = c.abort(c.enclosingPosition, s)

    import c.universe._
    private[upickle] def companionTree(tpe: c.Type): Tree = {
      val companionSymbol = tpe.typeSymbol.companionSymbol

      if (companionSymbol == NoSymbol && tpe.typeSymbol.isClass) {
        val clsSymbol = tpe.typeSymbol.asClass
        val msg = "[error] The companion symbol could not be determined for " +
          s"[[${clsSymbol.name}]]. This may be due to a bug in scalac (SI-7567) " +
          "that arises when a case class within a function is upickle. As a " +
          "workaround, move the declaration to the module-level."
        fail(msg)
      } else {
        val symTab = c.universe.asInstanceOf[reflect.internal.SymbolTable]
        val pre = tpe.asInstanceOf[symTab.Type].prefix.asInstanceOf[Type]
        c.universe.internal.gen.mkAttributedRef(pre, companionSymbol)
      }

    }

    /**
      * If a super-type is generic, find all the subtypes, but at the same time
      * fill in all the generic type parameters that are based on the super-type's
      * concrete type
      */
    private def fleshedOutSubtypes(tpe: Type) = {
      for{
        subtypeSym <- tpe.typeSymbol.asClass.knownDirectSubclasses.filter(!_.toString.contains("<local child>"))
        if subtypeSym.isType
        st = subtypeSym.asType.toType
        baseClsArgs = st.baseType(tpe.typeSymbol).asInstanceOf[TypeRef].args
      } yield {
        tpe match{
          case ExistentialType(_, TypeRef(_, _, args)) =>
            st.substituteTypes(baseClsArgs.map(_.typeSymbol), args)
          case ExistentialType(_, _) => st
          case TypeRef(_, _, args) =>
            st.substituteTypes(baseClsArgs.map(_.typeSymbol), args)
        }
      }
    }

    private def deriveObject(tpe: c.Type) = {
      val mod = tpe.typeSymbol.asClass.module
      val symTab = c.universe.asInstanceOf[reflect.internal.SymbolTable]
      val pre = tpe.asInstanceOf[symTab.Type].prefix.asInstanceOf[Type]
      val mod2 = c.universe.internal.gen.mkAttributedRef(pre, mod)

      annotate(tpe)(wrapObject(mod2))
    }

    private[upickle] def mergeTrait(tagKey: Option[String], subtrees: Seq[Tree], subtypes: Seq[Type], targetType: c.Type): Tree

    private[upickle] def derive(tpe: c.Type) = {
      if (tpe.typeSymbol.asClass.isTrait || (tpe.typeSymbol.asClass.isAbstract && !tpe.typeSymbol.isJava)) {
        val derived = deriveTrait(tpe)
        derived
      }
      else if (tpe.typeSymbol.isModuleClass) deriveObject(tpe)
      else deriveClass(tpe)
    }

    private def deriveTrait(tpe: c.Type): c.universe.Tree = {
      val clsSymbol = tpe.typeSymbol.asClass

      if (!clsSymbol.isSealed) {
        fail(s"[error] The referenced trait [[${clsSymbol.name}]] must be sealed.")
      }else if (clsSymbol.knownDirectSubclasses.filter(!_.toString.contains("<local child>")).isEmpty) {
        val msg =
          s"The referenced trait [[${clsSymbol.name}]] does not have any sub-classes. This may " +
            "happen due to a limitation of scalac (SI-7046). To work around this, " +
            "try manually specifying the sealed trait picklers as described in " +
            "https://com-lihaoyi.github.io/upickle/#ManualSealedTraitPicklers"
        fail(msg)
      }else{
        val tagKey = customKey(clsSymbol)
        val subTypes = fleshedOutSubtypes(tpe).toSeq.sortBy(_.typeSymbol.fullName)
        //    println("deriveTrait")
        val subDerives = subTypes.map(subCls => q"implicitly[${typeclassFor(subCls)}]")
        //    println(Console.GREEN + "subDerives " + Console.RESET + subDrivess)
        val merged = mergeTrait(tagKey, subDerives, subTypes, tpe)
        merged
      }
    }

    private[upickle] def typeclass: c.WeakTypeTag[M[_]]

    private def typeclassFor(t: Type) = {
      //    println("typeclassFor " + weakTypeOf[M[_]](typeclass))

      weakTypeOf[M[_]](typeclass) match {
        case TypeRef(a, b, _) =>
          internal.typeRef(a, b, List(t))
        case ExistentialType(_, TypeRef(a, b, _)) =>
          internal.typeRef(a, b, List(t))
        case x =>
          println("Dunno Wad Dis Typeclazz Is " + x)
          println(x)
          println(x.getClass)
          ???
      }
    }

    private def deriveClass(tpe: c.Type) = {
      val fields = getFields(tpe)
      // According to @retronym, this is necessary in order to force the
      // default argument `apply$default$n` methods to be synthesized
      val companion = companionTree(tpe)
      companion.tpe.member(TermName("apply")).info

      validateFlattenAnnotation(fields)
      val (mappedNames, types, defaultValues) = fields.toArray.filter { case (_, _, _, _, _, isCollection) => !isCollection }.map {
        case (_, mappedName, tpe, _, defaultValue, _) => (mappedName, tpe, defaultValue)
      }.unzip3
      val collectionFlattened = fields.find { case (_, _, _, _, _, isCollection) => isCollection }.map(_._3)

      val derive =
        // Otherwise, reading and writing are kinda identical
        wrapCaseN(
          mappedNames,
          types,
          defaultValues,
          collectionFlattened,
          targetType = tpe,
        )

      annotate(tpe)(derive)
    }

    private def getFields(tpe: c.Type): List[(String, String, c.Type, Symbol, Option[c.Tree], Boolean)] = {
      val params = getSymbols(tpe)
      val fields = params.zipWithIndex.flatMap { case (param, i) =>
        val (mappedName, tpeOfField, defaultValue) = getSymbolDetail(param, i, tpe)
        param.annotations.find(_.tree.tpe =:= typeOf[flatten]) match {
          case Some(_) =>
            if (isCollectionFlattenable(tpeOfField))
              List((param.name.toString, mappedName, tpeOfField, param, defaultValue, true))
            else if (tpeOfField.typeSymbol.isClass && tpeOfField.typeSymbol.asClass.isCaseClass) {
              getFields(tpeOfField)
            }
            else fail(s"Invalid type for flattening: $tpeOfField.")
          case None =>
            List((param.name.toString, mappedName, tpeOfField, param, defaultValue, false))
        }
      }
      fields
    }

    private[upickle] def getSymbols(tpe: c.Type): List[Symbol] =  {
      val companion = companionTree(tpe)
      //tickle the companion members -- Not doing this leads to unexpected runtime behavior
      //I wonder if there is an SI related to this?
      companion.tpe.members.foreach(_ => ())

      tpe.members.find(x => x.isMethod && x.asMethod.isPrimaryConstructor) match {
        case Some(primaryConstructor) => primaryConstructor.asMethod.paramLists.flatten
        case None => fail("Can't find primary constructor of " + tpe)
      }
    }

    private[upickle] def getSymbolDetail(symbol: Symbol, idx: Int, containingTpe: c.Type): (String, c.Type, Option[c.Tree]) = {
      val name = symbol.name.decodedName.toString
      val mappedName = customKey(symbol).getOrElse(name)
      val companion = companionTree(containingTpe)
      val tpe = applyTypeArguments(containingTpe, appliedTpe = symbol.typeSignature)
      val defaultValue = if (symbol.asTerm.isParamWithDefault)
        Some(q"$companion.${TermName("apply$default$" + (idx + 1))}")
      else
        None
      (mappedName, tpe, defaultValue)
    }

    private def applyTypeArguments(tpe: c.Type, appliedTpe: c.Type): c.Type = {
      val typeParams = tpe.typeSymbol.asClass.typeParams
      val typeArguments = tpe.dealias.asInstanceOf[TypeRef].args
      if (appliedTpe.typeSymbol != definitions.RepeatedParamClass) {
        appliedTpe.substituteTypes(typeParams, typeArguments)
      } else {
        val TypeRef(pref, sym, _) = typeOf[Seq[Int]]
        internal.typeRef(pref, sym, appliedTpe.asInstanceOf[TypeRef].args)
      }
    }

    private def validateFlattenAnnotation(fields: List[(String, String, c.Type, Symbol, Option[c.Tree], Boolean)]): Unit = {
      if (fields.count { case(_, _, _, _, _, isCollection) => isCollection } > 1) {
        fail("Only one collection can be annotated with @upickle.implicits.flatten in the same level")
      }
      val duplicatedKeys = fields.map { case (_, mappedName, _, _, _, _) => mappedName }.groupBy(identity).collect { case (x, List(_, _, _*)) => x }
      if (duplicatedKeys.nonEmpty) {
        fail(
          s"""There are multiple fields with the same key.
             |Following keys are duplicated: ${duplicatedKeys.mkString(", ")}.
             |""".stripMargin)
      }
    }

    /** If there is a sealed base class, annotate the derived tree in the JSON
      * representation with a class label.
      */
    private def annotate(tpe: c.Type)(derived: c.universe.Tree) = {
      val sealedParents = tpe.baseClasses.filter(_.asClass.isSealed)

      if (sealedParents.isEmpty) derived
      else {
        val tagKey = MacrosCommon.tagKeyFromParents(
          tpe.typeSymbol.name.toString,
          sealedParents,
          customKey,
          (_: c.Symbol).name.toString,
          fail,
        )

        val sealedClassSymbol: Option[Symbol] = sealedParents.find(_ == tpe.typeSymbol)
        val segments =
          sealedClassSymbol.toList.map(_.fullName.split('.')) ++
            sealedParents
            .flatMap(_.asClass.knownDirectSubclasses)
            .map(_.fullName.split('.'))


        // -1 because even if there is only one subclass, and so no name segments
        // are needed to differentiate between them, we want to keep at least
        // the rightmost name segment
        val identicalSegmentCount = Range(0, segments.map(_.length).max - 1)
          .takeWhile(i => segments.map(_.lift(i)).distinct.size == 1)
          .length

        val tagValue = customKey(tpe.typeSymbol)
          .getOrElse(TypeName(tpe.typeSymbol.fullName).decodedName.toString)

        val shortTagValue = customKey(tpe.typeSymbol)
          .getOrElse(
            TypeName(
              tpe.typeSymbol.fullName.split('.').drop(identicalSegmentCount).mkString(".")
            ).decodedName.toString
          )

        val tagKeyExpr = tagKey match {
          case Some(v) => q"$v"
          case None => q"${c.prefix}.tagName"
        }
        q"${c.prefix}.annotate($derived, $tagKeyExpr, $tagValue, $shortTagValue)"
      }
    }

    private[upickle] def isCollectionFlattenable(tpe: c.Type): Boolean =
        tpe <:< typeOf[Iterable[(_, _)]]

    private[upickle] def extractKeyValueTypes(tpe: c.Type): (Symbol, c.Type, c.Type) =
      tpe match {
        case TypeRef(_, collection, keyType :: valueType :: Nil) => (collection, keyType, valueType)
        case TypeRef(_, collection, TypeRef(_, _, keyType :: valueType :: Nil) :: Nil) => (collection, keyType, valueType)
        case _ => fail(s"Fail to extract key value from $tpe")
      }

    private def customKey(sym: c.Symbol): Option[String] = {
        sym.annotations
          .find(_.tree.tpe == typeOf[key])
          .flatMap(_.tree.children.tail.headOption)
          .map{case Literal(Constant(s)) => s.toString}
    }

    private[upickle] def serializeDefaults(sym: c.Symbol): Option[Boolean] = {
        sym.annotations
          .find(_.tree.tpe == typeOf[upickle.implicits.serializeDefaults])
          .flatMap(_.tree.children.tail.headOption)
          .map{case Literal(Constant(s)) => s.asInstanceOf[Boolean]}
    }

    private[upickle] def wrapObject(obj: Tree): Tree

    private[upickle] def wrapCaseN(mappedNames: Array[String],
                                   types: Array[c.Type],
                                   defaultValues: Array[Option[c.Tree]],
                                   collectionFlattened: Option[c.Type],
                                   targetType: c.Type): Tree
  }

  private[upickle] abstract class Reading[M[_]] extends DeriveDefaults[M] {
    val c: scala.reflect.macros.blackbox.Context
    import c.universe._
    def wrapObject(t: c.Tree) = q"new ${c.prefix}.SingletonReader($t)"

    def wrapCaseN(mappedNames: Array[String],
                  types: Array[c.Type],
                  defaultValues: Array[Option[c.Tree]],
                  collectionFlattened: Option[c.Type],
                  targetType: c.Type): c.universe.Tree = {
      val allowUnknownKeysAnnotation = targetType.typeSymbol
        .annotations
        .find(_.tree.tpe == typeOf[upickle.implicits.allowUnknownKeys])
        .flatMap(_.tree.children.tail.headOption)
        .map { case Literal(Constant(b)) => b.asInstanceOf[Boolean] }

      val (hasFlattenOnCollection, _, keyTypeOfCollection, valueTypeOfCollection) = collectionFlattened match {
        case Some(tpe) =>
          val (collection, key, value) = extractKeyValueTypes(tpe)
          (true, collection, key, value)
        case None => (false, NoSymbol, NoType, NoType)
      }
      val numberOfFields = mappedNames.length
      val (localReaders, aggregates) = mappedNames.zipWithIndex.map { case (_, idx) =>
            (TermName(s"localReader$idx"), TermName(s"aggregated$idx"))
        }.unzip

      def constructClass(constructedTpe: c.Type): c.universe.Tree = {
        def loop(tpe: c.Type, offset: Int): (c.universe.Tree, Int) = {
          val companion = companionTree(tpe)
          val symbols = getSymbols(tpe)
          val varArgs = symbols.lastOption.exists(_.typeSignature.typeSymbol == definitions.RepeatedParamClass)
          val (terms, nextOffset) =
            symbols.foldLeft((List.empty[Tree], offset)) { case ((terms, idx), symbol) =>
              val (_, tpeOfField, _) = getSymbolDetail(symbol, 0, tpe)
              symbol.annotations.find(_.tree.tpe =:= typeOf[flatten]) match {
                case Some(_) =>
                  if (isCollectionFlattenable(tpeOfField)) {
                    val termName = TermName(s"aggregatedCollection")
                    val companion = companionTree(tpeOfField)
                    val term = q"$companion($termName.toList :_*)"
                    (term :: terms, idx)
                  } else if (tpeOfField.typeSymbol.isClass && tpeOfField.typeSymbol.asClass.isCaseClass) {
                    val (term, nextIdx) = loop(tpeOfField, idx)
                    (term :: terms, nextIdx)
                  }
                  else fail(s"Invalid type for flattening: $tpeOfField.")
                case None =>
                  val termName = TermName(s"aggregated$idx")
                  val term = if (symbol == symbols.last && varArgs) q"$termName:_*" else q"$termName"
                  (term :: terms, idx + 1)
              }
            }
          val constructed =
            q"""
              $companion.apply(
                ..${
                  terms.reverse
                }
              )
            """
          (constructed, nextOffset)
        }
        loop(constructedTpe, 0)._1
      }

      val visitValueDef =
        if (numberOfFields <= 64)
          q"""
            override def visitValue(v: Any, index: Int): Unit = {
              if ((currentIndex != -1) && ((found & (1L << currentIndex)) == 0)) {
                storeAggregatedValue(currentIndex, v)
                found |= (1L << currentIndex)
              }
              else if (storeToMap) {
                storeAggregatedValue(currentIndex, v)
              }
            }
          """
        else
          q"""
            override def visitValue(v: Any, index: Int): Unit = {
              if ((currentIndex != -1) && ((found(currentIndex / 64) & (1L << currentIndex)) == 0)) {
                storeAggregatedValue(currentIndex, v)
                found(currentIndex / 64) |= (1L << currentIndex)
              }
              else if (storeToMap) {
                storeAggregatedValue(currentIndex, v)
              }
            }
          """

      q"""
        ..${
          for (i <- types.indices)
          yield q"private[this] lazy val ${localReaders(i)} = implicitly[${c.prefix}.Reader[${types(i)}]]"
        }
        ..${
          if (hasFlattenOnCollection)
            List(
              q"private[this] lazy val localReaderCollectionKey = implicitly[${c.prefix}.Reader[$keyTypeOfCollection]]",
              q"private[this] lazy val localReaderCollectionValue = implicitly[${c.prefix}.Reader[$valueTypeOfCollection]]",
            )
          else Nil
        }
        new ${c.prefix}.CaseClassReader[$targetType] {
          override def visitObject(length: Int, jsonableKeys: Boolean, index: Int) = new ${if (numberOfFields <= 64) tq"_root_.upickle.implicits.CaseObjectContext2[$targetType]" else tq"_root_.upickle.implicits.HugeCaseObjectContext2[$targetType]"}(${numberOfFields}) {
            ..${
              if (hasFlattenOnCollection)
                List(q"var currentKey: $keyTypeOfCollection = _")
              else
                List(q"var currentKey: String = _")
            }
            var storeToMap = false

            $visitValueDef

            ..${
              for (i <- types.indices)
              yield q"private[this] var ${aggregates(i)}: ${types(i)} = _"
            }
            ..${
              if (hasFlattenOnCollection)
                List(
                  q"private[this] lazy val aggregatedCollection: scala.collection.mutable.ListBuffer[($keyTypeOfCollection, $valueTypeOfCollection)] = scala.collection.mutable.ListBuffer.empty",
                )
              else Nil
            }

            ..${
              if (hasFlattenOnCollection)
                List(q"override def visitKey(index: Int): _root_.upickle.core.Visitor[_, _] = _root_.upickle.core.BufferedValue.Builder")
              else Nil
            }

            def storeAggregatedValue(currentIndex: Int, v: Any): Unit = currentIndex match {
              case ..${
                for (i <- aggregates.indices)
                  yield cq"$i => ${aggregates(i)} = v.asInstanceOf[${types(i)}]"
                }
              case ..${
                  if (hasFlattenOnCollection)
                    List(cq"-1 => aggregatedCollection += currentKey -> v.asInstanceOf[$valueTypeOfCollection]")
                  else Nil
                }
              case _ => throw new java.lang.IndexOutOfBoundsException(currentIndex.toString)
            }

            def visitKeyValue(s: Any) = {
              storeToMap = false
              val currentKeyString = ${
                if (hasFlattenOnCollection)
                  q"${c.prefix}.objectAttributeKeyReadMap(_root_.upickle.core.BufferedValue.valueToSortKey(s.asInstanceOf[_root_.upickle.core.BufferedValue])).toString"
                else
                  q"${c.prefix}.objectAttributeKeyReadMap(s.toString).toString"
              }
              currentIndex = currentKeyString match {
                case ..${
                  for (i <- mappedNames.indices)
                    yield cq"${mappedNames(i)} => $i"
                }
                case _ =>
                  ${
                    (allowUnknownKeysAnnotation, hasFlattenOnCollection) match {
                      case (_, true) => q"""
                        currentKey = _root_.upickle.core.BufferedValue.transform(s.asInstanceOf[_root_.upickle.core.BufferedValue], localReaderCollectionKey).asInstanceOf[$keyTypeOfCollection]
                        storeToMap = true
                        -1
                      """
                      case (None, false) =>
                        q"""
                          if (${ c.prefix }.allowUnknownKeys) -1
                          else throw new _root_.upickle.core.Abort("Unknown Key: " + s.toString)
                        """
                      case (Some(false), false) => q"""throw new _root_.upickle.core.Abort(" Unknown Key: " + s.toString)"""
                      case (Some(true), false) => q"-1"
                    }
                  }
              }
            }

            def visitEnd(index: Int) = {
              ..${
                for(i <- defaultValues.indices if defaultValues(i).isDefined)
                  yield q"this.storeValueIfNotFound($i, ${defaultValues(i).get})"
              }

              // Special-case 64 because java bit shifting ignores any RHS values above 63
              // https://docs.oracle.com/javase/specs/jls/se7/html/jls-15.html#jls-15.19
              if (${
                if (numberOfFields <= 64) q"this.checkErrorMissingKeys(${if (numberOfFields == 64) -1 else (1L << numberOfFields) - 1})"
                else q"this.checkErrorMissingKeys(${numberOfFields})"
              }) {
                this.errorMissingKeys(${numberOfFields}, ${mappedNames})
              }

              ${constructClass(targetType)}
            }

            def subVisitor: _root_.upickle.core.Visitor[_, _] = currentIndex match {
              case -1 =>
                ${
                  if (hasFlattenOnCollection)
                    q"localReaderCollectionValue"
                  else
                    q"_root_.upickle.core.NoOpVisitor"
                }
              case ..${
                      for (i <- mappedNames.indices)
                        yield cq"$i => ${localReaders(i)} "
                    }
              case _ => throw new java.lang.IndexOutOfBoundsException(currentIndex.toString)
            }
          }
        }
      """
    }

    override def mergeTrait(tagKey: Option[String], subtrees: Seq[Tree], subtypes: Seq[Type], targetType: c.Type): Tree = {
      val tagKeyExpr = tagKey match {
        case Some(v) => q"$v"
        case None => q"${c.prefix}.tagName"
      }
      q"${c.prefix}.Reader.merge[$targetType]($tagKeyExpr, ..$subtrees)"
    }
  }

  private[upickle] abstract class Writing[M[_]] extends DeriveDefaults[M] {
    val c: scala.reflect.macros.blackbox.Context
    import c.universe._
    def wrapObject(obj: c.Tree) = q"new ${c.prefix}.SingletonWriter($obj)"

    def internal = q"${c.prefix}.Internal"

    def wrapCaseN(mappedNames: Array[String],
                  types: Array[c.Type],
                  defaultValues: Array[Option[c.Tree]],
                  collectionFlattened: Option[c.Type],
                  targetType: c.Type): c.universe.Tree = {
      def serDfltVals(symbol: Symbol) = {
        val b: Option[Boolean] = serializeDefaults(symbol).orElse(serializeDefaults(targetType.typeSymbol))
        b match {
          case Some(b) => q"${b}"
          case None => q"${c.prefix}.serializeDefaults"
        }
      }

      def write(tpe: c.Type, outer: c.universe.Tree): List[c.universe.Tree] = {
        val symbols = getSymbols(tpe)
        symbols.zipWithIndex.flatMap { case (symbol, i) =>
          val (mappedName, tpeOfField, defaultValue) = getSymbolDetail(symbol, i, tpe)
          val select = Select(outer, TermName(symbol.name.toString))

          symbol.annotations.find(_.tree.tpe =:= typeOf[flatten]) match {
            case Some(_) =>
              if (isCollectionFlattenable(tpeOfField)) {
                val (_, keyType, valueType) = extractKeyValueTypes(tpeOfField)
                q"""
                  val collisions = allKeys.intersect($select.map(_._1.toString).toSet)
                  if (collisions.nonEmpty) {
                    throw new Exception("Key collision detected for the following keys: " + collisions.mkString(", "))
                  }
                  val keyWriter = implicitly[${c.prefix}.Writer[$keyType]]
                  val valueWriter = implicitly[${c.prefix}.Writer[$valueType]]
                  $select.foreach { case (key, value) =>
                    val keyVisitor = ctx.visitKey(-1)
                    ctx.visitKeyValue(keyWriter.write(keyVisitor, key))
                    ctx.narrow.visitValue(valueWriter.write(ctx.subVisitor, value), -1)
                  }
                """ :: Nil
              } else if (tpeOfField.typeSymbol.isClass && tpeOfField.typeSymbol.asClass.isCaseClass) {
                write(tpeOfField, select)
              }
              else fail(s"Invalid type for flattening: $tpeOfField.")
            case None =>
              val snippet =
                q"""
                  this.writeSnippetMappedName[R, ${tpeOfField}](
                     ctx,
                     ${c.prefix}.objectAttributeKeyWriteMap(${mappedName}),
                     implicitly[${c.prefix}.Writer[${tpeOfField}]],
                     $select
                   )
                """
              val default = if (defaultValue.isEmpty) snippet
              else q"""if (${serDfltVals(symbol)} || $select != ${defaultValue.get}) $snippet"""
              default :: Nil
          }
        }
      }

      def getLength(tpe: c.Type, outer: c.universe.Tree): List[c.universe.Tree] = {
        val symbols = getSymbols(tpe)
        symbols.zipWithIndex.flatMap { case (symbol, i) =>
          val (_, tpeOfField, defaultValue) = getSymbolDetail(symbol, i, tpe)
          val select = Select(outer, TermName(symbol.name.toString))
          symbol.annotations.find(_.tree.tpe =:= typeOf[flatten]) match {
            case Some(_) =>
              if (isCollectionFlattenable(tpeOfField)) {
                q"${select}.size" :: Nil
              }
              else if (tpeOfField.typeSymbol.isClass && tpeOfField.typeSymbol.asClass.isCaseClass) {
                getLength(tpeOfField, select)
              }
              else fail(s"Invalid type for flattening: $tpeOfField.")
            case None =>
              val snippet = if (defaultValue.isEmpty) q"1"
              else q"""if (${serDfltVals(symbol)} || $select != ${defaultValue.get}) 1 else 0"""
              snippet :: Nil
          }
        }
      }

      q"""
        new ${c.prefix}.CaseClassWriter[$targetType]{
          private lazy val allKeys = Set[String](..${mappedNames.toList.map(name => Literal(Constant(name)))})

          def length(v: $targetType) = {
            ${
              getLength(targetType, q"v")
                .foldLeft[Tree](q"0") { case (prev, next) => q"$prev + $next" }
            }
          }
          override def write0[R](out: _root_.upickle.core.Visitor[_, R], v: $targetType): R = {
            if (v == null) out.visitNull(-1)
            else {
              val ctx = out.visitObject(length(v), true, -1)
              ..${write(targetType, q"v")}
              ctx.visitEnd(-1)
            }
          }
          def writeToObject[R](ctx: _root_.upickle.core.ObjVisitor[_, R],
                               v: $targetType): Unit = {
            ..${write(targetType, q"v")}
          }
        }
      """
    }

    override def mergeTrait(tagKey: Option[String], subtree: Seq[Tree], subtypes: Seq[Type], targetType: c.Type): Tree = {
      q"${c.prefix}.Writer.merge[$targetType](..$subtree)"
    }
  }
  def macroRImpl[T, R[_]](c0: scala.reflect.macros.blackbox.Context)
                         (implicit e1: c0.WeakTypeTag[T], e2: c0.WeakTypeTag[R[_]]): c0.Expr[R[T]] = {
    import c0.universe._
    val res = new Reading[R]{
      val c: c0.type = c0
      def typeclass = e2
    }.derive(e1.tpe)
//    println(c0.universe.showCode(res))
    c0.Expr[R[T]](res)
  }

  def macroWImpl[T, W[_]](c0: scala.reflect.macros.blackbox.Context)
                         (implicit e1: c0.WeakTypeTag[T], e2: c0.WeakTypeTag[W[_]]): c0.Expr[W[T]] = {
    import c0.universe._
    val res = new Writing[W]{
      val c: c0.type = c0
      def typeclass = e2
    }.derive(e1.tpe)
//    println(c0.universe.showCode(res))
    c0.Expr[W[T]](res)
  }
}

