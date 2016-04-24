package derive

import ScalaVersionStubs._
import acyclic.file
import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import language.higherKinds
/**
 * Used to annotate either case classes or their fields, telling derive
 * to use a custom string as the key for that class/field rather than the
 * default string which is the full-name of that class/field.
 */
case class key(s: String) extends StaticAnnotation
trait DeriveApi[M[_]]{
  val c: Context
  import c.universe._
  def typeclass: c.WeakTypeTag[M[_]]
  def wrapObject(obj: Tree): Tree
  def wrapCase0(companion: Tree, targetType: c.Type): Tree
  def wrapCase1(companion: Tree, arg: String, typeArgs: Seq[c.Type], argTypes: Type, targetType: c.Type): Tree
  def wrapCaseN(companion: Tree, args: Seq[String], typeArgs: Seq[c.Type], argTypes: Seq[Type],targetType: c.Type): Tree
  def knot(t: Tree): Tree
  def mergeTrait(subtree: Seq[Tree], subtypes: Seq[Type], targetType: c.Type): Tree
  def fallbackDerivation(t: Type): Option[Tree] = None
}

abstract class Derive[M[_]] extends DeriveApi[M]{
  case class TypeKey(t: c.Type) {
    override def equals(o: Any) = t =:= o.asInstanceOf[TypeKey].t
    override def hashCode: Int = t.typeSymbol.fullName.hashCode
  }
  import c.universe._
  import compat._
  def fail(tpe: Type, s: String): Tree = fallbackDerivation(tpe) match{
    case None => c.abort(c.enclosingPosition, s)
    case Some(tree) => tree
  }
  def typeclassFor(t: Type) = {
//    println("typeclassFor " + weakTypeOf[M[_]](typeclass))

    weakTypeOf[M[_]](typeclass) match {
      case TypeRef(a, b, _) =>
        TypeRef(a, b, List(t))
      case ExistentialType(_, TypeRef(a, b, _)) =>
        TypeRef(a, b, List(t))
      case x =>
        println("Dunno Wad Dis Typeclazz Is " + x)
        println(x)
        println(x.getClass)
        ???
    }
  }
  def freshName = c.fresh[TermName]("derive")


  def derive[T: c.WeakTypeTag] = {
    val tpe = weakTypeTag[T].tpe
//    println(Console.YELLOW + "derive " + tpe + Console.RESET)
    val res =
      if (tpe != typeOf[Nothing]) deriveType(tpe, true)
      else fail(tpe, "Inferred `Reader[Nothing]`, something probably went wrong")
//    println(Console.YELLOW + res + Console.RESET)
    res
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

  def isAccessible(tpe: Type): Boolean = {
//    println("isAccessible " + tpe)
    def check(pre: Type, sym: Symbol) = {
      val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
      val typer = c.asInstanceOf[reflect.macros.runtime.Context].callsiteTyper.asInstanceOf[global.analyzer.Typer]
      val typerContext = typer.context
      val sym = tpe.typeSymbol
      val res =  typerContext.isAccessible(
        sym.asInstanceOf[global.Symbol],
        pre.asInstanceOf[global.Type]
      )
      res && isAccessible(pre)
    }
    val res = tpe match{
      case t: TypeRef => check(t.pre, t.sym)
      case t: SingleType => check(t.pre, t.sym)
      case t: ThisType => isAccessible(t.typeSymbol.asType.toType)
      case t: ExistentialType => isAccessible(t.underlying)
      case _ => true
    }
//    println("isAccessible " + tpe + " " + res)
    res
  }
  /**
   * derive the typeclass for a particular type
   */
  def deriveType(tpe: c.Type, first: Boolean): c.Tree = {
    // println(Console.CYAN + "derive " + Console.RESET + tpe + " " + System.identityHashCode(tpe))
    val memo = collection.mutable.Map.empty[TypeKey, Map[TypeKey, TermName]]

    val seen = collection.mutable.Set.empty[TypeKey]
    def implicited(tpe: Type) = q"implicitly[${typeclassFor(tpe)}]"


    def onFail(tpe: Type, key: TypeKey, name: TermName): Map[TypeKey, TermName] = {

      tpe.normalize match {
        case x if !isAccessible(tpe) =>
          // println("<Not Accessible>" + x)
          Map()
        case TypeRef(_, cls, args) if cls == definitions.RepeatedParamClass =>
          // println(Console.CYAN + "<Repeat>" + Console.RESET + tpe)
          rec(args(0))
        case TypeRef(pref, cls, args)
          if tpe.typeSymbol.isClass
            && (tpe.typeSymbol.asClass.isTrait || tpe.typeSymbol.asClass.isAbstractClass)
            && tpe.typeSymbol.asClass.isSealed =>
          // println(Console.CYAN + "<Traitish>" + Console.RESET + tpe)
          val subTypes = fleshedOutSubtypes(tpe.asInstanceOf[TypeRef])

          val lol =
            Map(key -> name) ++
              subTypes.flatMap(rec(_)) ++
              args.flatMap(rec(_))

          lol
        case tpe if tpe.typeSymbol.isModuleClass =>
          // println(Console.CYAN + "<Singleton>" + Console.RESET + tpe)
          Map(key -> name)
        case TypeRef(_, cls, args) if cls.isClass && !cls.asClass.isAbstractClass =>
          // println(Console.CYAN + "<Class>" + Console.RESET + tpe)
          getArgSyms(tpe) match {
            case Left(errMsg) =>
              // println("LEFT")
              Map.empty[TypeKey, TermName]
            case Right((companion, typeParams, argSyms)) =>
              // println("Right")
              val x =
                argSyms
                  .map(_.typeSignature.substituteTypes(typeParams, args))
                  .flatMap(rec(_))
                  .toSet

              Map(key -> name) ++ x
          }

        case x =>
          // println("<???>")
          Map()
      }

    }
    def rec(tpe0: c.Type, name: TermName = freshName, first: Boolean = false): Map[TypeKey, TermName] = {
      val tpe = removeRepeats(tpe0)
      // println(Console.CYAN + "REC " + Console.RESET + tpe + "\t" + first)
      val key = TypeKey(tpe)
      // println(Console.CYAN + seen + Console.RESET + " " + System.identityHashCode(seen))
      // println(Console.CYAN + memo + Console.RESET + " " + System.identityHashCode(memo))
      if (seen(TypeKey(tpe))) Map()
      else {
        seen.add(key)
        memo.getOrElseUpdate(TypeKey(tpe), {
          // If it can't find any non-macro implicits, try to recurse into the type
          val dummies = tpe match {
            case TypeRef(_, _, args) =>
              args.map(TypeKey)
                .distinct
                .map(_.t)
                .filter{ t =>
                    val x = c.inferImplicitValue(typeclassFor(t), withMacrosDisabled = true)
                  x == EmptyTree
                }
                .map(tpe => q"implicit def $freshName: ${typeclassFor(tpe)} = ???")
            case _ =>
              Seq.empty[Tree]
          }
          val classTagImplicit = freshName
          val classTagImplicitType = c.fresh[TypeName]("derive")
          // Hard-code availability of ClassTags to make array serialization work
          val probe = q"""{
              ..$dummies;
              implicit def $classTagImplicit[$classTagImplicitType]: reflect.ClassTag[$classTagImplicitType] = ???;
              ${implicited(tpe)}
            }"""
          // println("TC " + name + " " + probe)
          if (first) {
            // println("FIRST")
            seen.add(key)
            onFail(tpe, key, name)
          }else c.typeCheck(probe, withMacrosDisabled = true, silent = true) match {
            case EmptyTree =>
              // println("Empty")
              seen.add(key)
              onFail(tpe, key, name)
            case t =>
              // println("Present")
              Map()
          }

        })
      }
    }

    //    println("a")
    val first = freshName
    //    println("b")
    val recTypes = rec(tpe, first, first = true)

    // println("cD")
    //      println("recTypes " + recTypes)

    val things = recTypes.map { case (TypeKey(tpe), name) =>
      val pick =
        if (tpe.typeSymbol.asClass.isTrait || (tpe.typeSymbol.asClass.isAbstractClass && !tpe.typeSymbol.isJava)) deriveTrait(tpe)
        else if (tpe.typeSymbol.isModuleClass) deriveObject(tpe)
        else deriveClass(tpe)

      q"""
          implicit lazy val $name: ${typeclassFor(tpe)} = ${knot(pick)}
        """

    }
    //        println(things)

    val returnName = freshName
    // Do this weird immediately-called-method dance to avoid weird crash
    // in 2.11.x:
    //
    // """
    // symbol variable bitmap$0 does not exist in derive.X.<init>
    // scala.reflect.internal.FatalError: symbol variable bitmap$0 does not exist in derive.X.<init>
    // """
    //
    // Dump it in an anonymous class to avoid https://issues.scala-lang.org/browse/SI-8775,
    // which results in this other weird crash
    //
    // Implementation restriction: access of value derive$macro$2$1 from <$anon: Function0>,
    // would require illegal premature access to the unconstructed `this` of class Something
    // in object Main
    val res = q"""
      (new {
        ..$things
        def $returnName = {
          ${
      recTypes.mapValues(x => q"$x")
        .getOrElse(TypeKey(tpe), fail(tpe, "Couldn't derive type " + tpe))
          }
        }

      }).$returnName
      """
      //            println("RES " + res)

      //    println(Console.CYAN + "end derive " + Console.RESET + tpe + " " + System.identityHashCode(tpe))
//    println(res)
    res
  }

  def deriveTrait(tpe: c.Type): c.universe.Tree = {
    val clsSymbol = tpe.typeSymbol.asClass

    if (!clsSymbol.isSealed) {
      fail(tpe, s"[error] The referenced trait [[${clsSymbol.name}]] must be sealed.")
    }else if (clsSymbol.knownDirectSubclasses.filter(!_.toString.contains("<local child>")).isEmpty) {
      val msg = s"The referenced trait [[${clsSymbol.name}]] does not have any sub-classes. This may " +
        "happen due to a limitation of scalac (SI-7046) given that the trait is " +
        "not in the same package. If this is the case, the hierarchy may be " +
        "defined using integer constants."
      fail(tpe, msg)
    }else{
      val subTypes = fleshedOutSubtypes(tpe.asInstanceOf[TypeRef]).toSeq
      //    println("deriveTrait")
      val subDerives = subTypes.map(subCls => q"implicitly[${typeclassFor(subCls)}]")
      //    println(Console.GREEN + "subDerives " + Console.RESET + subDrivess)
      mergeTrait(subDerives, subTypes, tpe)
    }
  }

  def deriveClass(tpe: c.Type) = {
    getArgSyms(tpe) match {
      case Left(msg) => fail(tpe, msg)
      case Right((companion, typeParams, argSyms)) =>

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
          if (args.length == 0) // 0-arg case classes are treated like `object`s
            wrapCase0(companion, tpe)
          else if (args.length == 1) // 1-arg case classes often need their output wrapped in a Tuple1
            wrapCase1(companion, args(0), typeArgs, func(argSyms(0).typeSignature), tpe)
          else // Otherwise, reading and writing are kinda identical
            wrapCaseN(companion, args, typeArgs, argSyms.map(_.typeSignature).map(func), tpe)

        annotate(tpe)(q"$derive")
    }
  }
  def removeRepeats(t: Type) = {
    if (t.typeSymbol == definitions.RepeatedParamClass) t.asInstanceOf[TypeRef].args(0)
    else t
  }

  def deriveObject(tpe: c.Type) = {
    val mod = tpe.typeSymbol.asClass.module
    val symTab = c.universe.asInstanceOf[reflect.internal.SymbolTable]
    val pre = tpe.asInstanceOf[symTab.Type].prefix.asInstanceOf[Type]
    val mod2 = c.universe.treeBuild.mkAttributedRef(pre, mod)

    annotate(tpe)(wrapObject(mod2))

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

  /**
   * Get the custom @key annotation from the parameter Symbol if it exists
   */
  def customKey(sym: c.Symbol): Option[String] = {
    sym.annotations
      .find(_.tpe == typeOf[key])
      .flatMap(_.scalaArgs.headOption)
      .map{case Literal(Constant(s)) => s.toString}
  }

  def getArgSyms(tpe: c.Type) = {
    companionTree(tpe).right.flatMap { companion =>
      //tickle the companion members -- Not doing this leads to unexpected runtime behavior
      //I wonder if there is an SI related to this?
      companion.tpe.members.foreach(_ => ())
      tpe.members.find(x => x.isMethod && x.asMethod.isPrimaryConstructor) match {
        case None => Left("Can't find primary constructor of " + tpe)
        case Some(primaryConstructor) => Right((companion, tpe.typeSymbol.asClass.typeParams, primaryConstructor.asMethod.paramss.flatten))
      }

    }
  }

  /**
   * Special haxx0rs to get us the real companion symbol using the
   * compiler internals, which isn't possible just using the public API
   */
  def companionTree(tpe: c.Type): Either[String, Tree] = {
    val companionSymbol = tpe.typeSymbol.companionSymbol

    if (companionSymbol == NoSymbol && tpe.typeSymbol.isClass) {
      val clsSymbol = tpe.typeSymbol.asClass
      val msg = "[error] The companion symbol could not be determined for " +
        s"[[${clsSymbol.name}]]. This may be due to a bug in scalac (SI-7567) " +
        "that arises when a case class within a function is derive. As a " +
        "workaround, move the declaration to the module-level."
      Left(msg)
    }else{
      val symTab = c.universe.asInstanceOf[reflect.internal.SymbolTable]
      val pre = tpe.asInstanceOf[symTab.Type].prefix.asInstanceOf[Type]
      Right(c.universe.treeBuild.mkAttributedRef(pre, companionSymbol))
    }

  }
}

//object Main extends scalajs.js.JSApp{
//  def main() = {
//    println(collection.mutable.Buffer().stringPrefix)
//  }
//}