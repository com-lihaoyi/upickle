package upickle

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import compat._
import acyclic.file
import language.higherKinds
import language.existentials
case class key(s: String) extends StaticAnnotation
/**
 * Implementation of macros used by uPickle to serialize and deserialize
 * case classes automatically. You probably shouldn't need to use these
 * directly, since they are called implicitly when trying to read/write
 * types you don't have a Reader/Writer in scope for.
 */
object Macros {

  trait DeriveDefaults[M[_]] {
    val c: scala.reflect.macros.blackbox.Context
    def fail(tpe: c.Type, s: String) = c.abort(c.enclosingPosition, s)

    import c.universe._
    def companionTree(tpe: c.Type): Either[String, Tree] = {
      val companionSymbol = tpe.typeSymbol.companionSymbol

      if (companionSymbol == NoSymbol && tpe.typeSymbol.isClass) {
        val clsSymbol = tpe.typeSymbol.asClass
        val msg = "[error] The companion symbol could not be determined for " +
          s"[[${clsSymbol.name}]]. This may be due to a bug in scalac (SI-7567) " +
          "that arises when a case class within a function is upickle. As a " +
          "workaround, move the declaration to the module-level."
        Left(msg)
      }else{
        val symTab = c.universe.asInstanceOf[reflect.internal.SymbolTable]
        val pre = tpe.asInstanceOf[symTab.Type].prefix.asInstanceOf[Type]
        Right(c.universe.treeBuild.mkAttributedRef(pre, companionSymbol))
      }

    }
    def getArgSyms(tpe: c.Type) = {
      companionTree(tpe).right.flatMap { companion =>
        //tickle the companion members -- Not doing this leads to unexpected runtime behavior
        //I wonder if there is an SI related to this?
        companion.tpe.members.foreach(_ => ())
        tpe.members.find(x => x.isMethod && x.asMethod.isPrimaryConstructor) match {
          case None => Left("Can't find primary constructor of " + tpe)
          case Some(primaryConstructor) =>
            val flattened = primaryConstructor.asMethod.paramss.flatten
            Right((
              companion,
              tpe.typeSymbol.asClass.typeParams,
              flattened,
              flattened.map(_.asTerm.isParamWithDefault)
            ))
        }

      }
    }

    def deriveDefaults(companion: c.Tree, hasDefaults: Seq[Boolean]): Seq[c.Tree] = {
      val defaults =
        for((hasDefault, i) <- hasDefaults.zipWithIndex)
        yield {
          val defaultName = newTermName("apply$default$" + (i + 1))
          if (!hasDefault) q"null"
          else q"${c.prefix}.writeJs($companion.$defaultName)"
        }
      defaults
    }
    /**
      * If a super-type is generic, find all the subtypes, but at the same time
      * fill in all the generic type parameters that are based on the super-type's
      * concrete type
      */
    def fleshedOutSubtypes(tpe: TypeRef) = {
      // Get ready to run this twice because for some reason scalac always
      // drops the type arguments from the subclasses the first time we
      // run this in 2.10.x
      def impl =
        for{
          subtypeSym <- tpe.typeSymbol.asClass.knownDirectSubclasses.filter(!_.toString.contains("<local child>"))
          if subtypeSym.isType
          st = subtypeSym.asType.toType
          baseClsArgs = st.baseType(tpe.typeSymbol).asInstanceOf[TypeRef].args
          // If the type arguments don't line up, just give up and fail to find
          // the subclasses. It should fall back to plain-old-toString
          if baseClsArgs.size == tpe.args.size
        } yield {

          val sub2 = st.substituteTypes(baseClsArgs.map(_.typeSymbol), tpe.args)
          //        println(Console.YELLOW + "sub2 " + Console.RESET + sub2)
          sub2
        }
      impl
      impl
    }

    def deriveObject(tpe: c.Type) = {
      val mod = tpe.typeSymbol.asClass.module
      val symTab = c.universe.asInstanceOf[reflect.internal.SymbolTable]
      val pre = tpe.asInstanceOf[symTab.Type].prefix.asInstanceOf[Type]
      val mod2 = c.universe.treeBuild.mkAttributedRef(pre, mod)

      annotate(tpe)(wrapObject(mod2))

    }
    def mergeTrait(subtrees: Seq[Tree], subtypes: Seq[Type], targetType: c.Type): Tree

    def derive(tpe: c.Type) = {
      if (tpe.typeSymbol.asClass.isTrait || (tpe.typeSymbol.asClass.isAbstractClass && !tpe.typeSymbol.isJava)) {
        val derived = deriveTrait(tpe)
        derived
      }
      else if (tpe.typeSymbol.isModuleClass) deriveObject(tpe)
      else deriveClass(tpe)
    }
    def deriveTrait(tpe: c.Type): c.universe.Tree = {
      val clsSymbol = tpe.typeSymbol.asClass

      if (!clsSymbol.isSealed) {
        fail(tpe, s"[error] The referenced trait [[${clsSymbol.name}]] must be sealed.")
      }else if (clsSymbol.knownDirectSubclasses.filter(!_.toString.contains("<local child>")).isEmpty) {
        val msg =
          s"The referenced trait [[${clsSymbol.name}]] does not have any sub-classes. This may " +
            "happen due to a limitation of scalac (SI-7046). To work around this, " +
            "try manually specifying the sealed trait picklers as described in " +
            "http://lihaoyi.github.com/upickle-pprint/upickle/#ManualSealedTraitPicklers"
        fail(tpe, msg)
      }else{
        val subTypes = fleshedOutSubtypes(tpe.asInstanceOf[TypeRef]).toSeq
        //    println("deriveTrait")
        val subDerives = subTypes.map(subCls => q"implicitly[${typeclassFor(subCls)}]")
        //    println(Console.GREEN + "subDerives " + Console.RESET + subDrivess)
        val merged = mergeTrait(subDerives, subTypes, tpe)
        merged
      }
    }

    def typeclass: c.WeakTypeTag[M[_]]

    def typeclassFor(t: Type) = {
      //    println("typeclassFor " + weakTypeOf[M[_]](typeclass))

      weakTypeOf[M[_]](typeclass) match {
        case TypeRef(a, b, _) =>
          import compat._
          TypeRef(a, b, List(t))
        case ExistentialType(_, TypeRef(a, b, _)) =>
          import compat._
          TypeRef(a, b, List(t))
        case x =>
          println("Dunno Wad Dis Typeclazz Is " + x)
          println(x)
          println(x.getClass)
          ???
      }
    }

    def deriveClass(tpe: c.Type) = {
      getArgSyms(tpe) match {
        case Left(msg) => fail(tpe, msg)
        case Right((companion, typeParams, argSyms, hasDefaults)) =>

          //    println("argSyms " + argSyms.map(_.typeSignature))
          val args = argSyms.map { p =>
            customKey(p).getOrElse(p.name.toString)
          }

          val typeArgs = tpe match {
            case TypeRef(_, _, args) => args
            case _ => c.abort(
              c.enclosingPosition,
              s"Don't know how to derive type $tpe"
            )
          }


          def func(t: Type) = {

            if (argSyms.length == 0) t
            else {
              val base = argSyms.map(_.typeSignature.typeSymbol)
              val concrete = tpe.normalize.asInstanceOf[TypeRef].args
              if (t.typeSymbol != definitions.RepeatedParamClass) {

                t.substituteTypes(typeParams, concrete)
              } else {
                val TypeRef(pref, sym, args) = typeOf[Seq[Int]]
                import compat._
                TypeRef(pref, sym, t.asInstanceOf[TypeRef].args)
              }
            }
          }

          val derive =
            if (args.isEmpty) // 0-arg case classes are treated like `object`s
              wrapCase0(companion, tpe)
            else if (args.length == 1) // 1-arg case classes often need their output wrapped in a Tuple1
              wrapCase1(
                companion,
                args(0),
                typeArgs,
                func(argSyms(0).typeSignature),
                hasDefaults(0),
                tpe
              )
            else // Otherwise, reading and writing are kinda identical
              wrapCaseN(
                companion,
                args,
                typeArgs,
                argSyms.map(_.typeSignature).map(func),
                hasDefaults,
                tpe
              )

          annotate(tpe)(derive)
      }
    }
    /** If there is a sealed base class, annotate the derived tree in the JSON
      * representation with a class label.
      */
    def annotate(tpe: c.Type)(derived: c.universe.Tree) = {
      val sealedParent = tpe.baseClasses.find(_.asClass.isSealed)
      sealedParent.fold(derived) { parent =>
        val index = customKey(tpe.typeSymbol).getOrElse(tpe.typeSymbol.fullName)
        q"${c.prefix}.annotate($derived, $index)"
      }
    }
    def customKey(sym: c.Symbol): Option[String] = {
        sym.annotations
          .find(_.tpe == typeOf[key])
          .flatMap(_.scalaArgs.headOption)
          .map{case Literal(Constant(s)) => s.toString}
    }

    def wrapObject(obj: Tree): Tree
    def wrapCase0(companion: Tree, targetType: c.Type): Tree
    def wrapCase1(companion: Tree,
                  arg: String,
                  typeArgs: Seq[c.Type],
                  argTypes: Type,
                  hasDefault: Boolean,
                  targetType: c.Type): Tree
    def wrapCaseN(companion: Tree,
                  args: Seq[String],
                  typeArgs: Seq[c.Type],
                  argTypes: Seq[Type],
                  hasDefaults: Seq[Boolean],
                  targetType: c.Type): Tree
  }

  abstract class Reading[M[_]] extends DeriveDefaults[M] {
    val c: scala.reflect.macros.blackbox.Context
    import c.universe._
    def wrapObject(t: c.Tree) = q"${c.prefix}.SingletonR($t)"
    def wrapCase0(t: c.Tree, targetType: c.Type) =
      q"${c.prefix}.${newTermName("Case0R")}(() => $t(): $targetType)"
    def wrapCase1(companion: c.Tree,
                  arg: String,
                  typeArgs: Seq[c.Type],
                  argType: c.Type,
                  hasDefault: Boolean,
                  targetType: c.Type) = {
      val defaults = deriveDefaults(companion, Seq(hasDefault))
      q"""
        ${c.prefix}.CaseR[_root_.scala.Tuple1[$argType], $targetType](
          _ match {case _root_.scala.Tuple1(x) => $companion.apply[..$typeArgs](x)},
          _root_.scala.Array($arg),
          _root_.scala.Array(..$defaults)
        )(${c.prefix}.Tuple1R)
        """
    }
    def wrapCaseN(companion: c.Tree,
                  args: Seq[String],
                  typeArgs: Seq[c.Type],
                  argTypes: Seq[Type],
                  hasDefaults: Seq[Boolean],
                  targetType: c.Type) = {
      val x = q"${c.fresh[TermName]("derive")}"
      val name = newTermName("Tuple"+args.length+"R")
      val argSyms = (1 to args.length).map(t => q"$x.${newTermName("_"+t)}")
      val defaults = deriveDefaults(companion, hasDefaults)
      q"""
        ${c.prefix}.CaseR[(..$argTypes), $targetType](
          ($x: (..$argTypes)) => ($companion.apply: (..$argTypes) => $targetType)(..$argSyms),
          _root_.scala.Array(..$args),
          _root_.scala.Array(..$defaults)
        )(${c.prefix}.$name)
      """
    }
    def mergeTrait(subtrees: Seq[Tree], subtypes: Seq[Type], targetType: c.Type): Tree = {

      q"${c.prefix}.Reader.merge[$targetType](..$subtrees)"
    }

  }
  abstract class Writing[M[_]] extends DeriveDefaults[M] {
    val c: scala.reflect.macros.blackbox.Context
    import c.universe._
    def wrapObject(obj: c.Tree) = q"${c.prefix}.SingletonW($obj)"
    def wrapCase0(companion: c.Tree, targetType: c.Type) = q"${c.prefix}.${newTermName("Case0W")}($companion.unapply)"
    def findUnapply(tpe: Type) = {
      val (companion, paramTypes, argSyms, hasDefaults) = getArgSyms(tpe).fold(
        errMsg => c.abort(c.enclosingPosition, errMsg),
        x => x
      )
      Seq("unapply", "unapplySeq")
        .map(newTermName(_))
        .find(companion.tpe.member(_) != NoSymbol)
        .getOrElse(c.abort(c.enclosingPosition, "None of the following methods " +
        "were defined: unapply, unapplySeq"))
    }
    def wrapCase1(companion: c.Tree,
                  arg: String,
                  typeArgs: Seq[c.Type],
                  argType: Type,
                  hasDefault: Boolean,
                  targetType: c.Type) = {
      val defaults = deriveDefaults(companion, Seq(hasDefault))
      q"""
        ${c.prefix}.CaseW[_root_.scala.Tuple1[$argType], $targetType](
          $companion.${findUnapply(targetType)}(_).map(_root_.scala.Tuple1.apply),
          _root_.scala.Array($arg),
          _root_.scala.Array(..$defaults)
        )(${c.prefix}.Tuple1W)
        """
    }

    def internal = q"${c.prefix}.Internal"
    def wrapCaseN(companion: c.Tree,
                  args: Seq[String],
                  typeArgs: Seq[c.Type],
                  argTypes: Seq[Type],
                  hasDefaults: Seq[Boolean],
                  targetType: c.Type) = {
      val defaults = deriveDefaults(companion, hasDefaults)
      val name = newTermName("Tuple"+args.length+"W")
      q"""
        ${c.prefix}.CaseW[(..$argTypes), $targetType](
          $companion.${findUnapply(targetType)}[..$typeArgs],
          _root_.scala.Array(..$args),
          _root_.scala.Array(..$defaults)
        )(${c.prefix}.$name)
      """
    }
    def mergeTrait(subtree: Seq[Tree], subtypes: Seq[Type], targetType: c.Type): Tree = {
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
    val msg = "Tagged Object " + weakTypeOf[T].typeSymbol.fullName
    c0.Expr[R[T]](q"""${c0.prefix}.Internal.validateReader($msg){$res}""")
  }

  def macroWImpl[T, W[_]](c0: scala.reflect.macros.blackbox.Context)
                         (implicit e1: c0.WeakTypeTag[T], e2: c0.WeakTypeTag[W[_]]): c0.Expr[W[T]] = {
    import c0.universe._
    val res = new Writing[W]{
      val c: c0.type = c0
      def typeclass = e2
    }.derive(e1.tpe)
    c0.Expr[W[T]](res)
  }

  def macroRWImpl[T, RM[_], WM[_]](c0: scala.reflect.macros.blackbox.Context)
                                  (implicit e1: c0.WeakTypeTag[T],
                                   e2: c0.WeakTypeTag[RM[_]],
                                   e3: c0.WeakTypeTag[WM[_]] ): c0.Expr[RM[T] with WM[T]] = {
    import c0.universe._
    val rRes = new Reading[RM]{
      val c: c0.type = c0
      def typeclass = e2
    }.derive(e1.tpe)

    val wRes = new Writing[WM]{
      val c: c0.type = c0
      def typeclass = e3
    }.derive(e1.tpe)

    val msg = "Tagged Object " + weakTypeOf[T].typeSymbol.fullName

    val res = c0.Expr[RM[T] with WM[T]](q"""${c0.prefix}.Internal.validateReaderWithWriter($msg)($rRes,$wRes)""")
    res
  }
}

