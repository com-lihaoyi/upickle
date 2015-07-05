package derive
import ScalaVersionStubs._
import acyclic.file
import scala.annotation.StaticAnnotation
import scala.language.experimental.macros

/**
 * Used to annotate either case classes or their fields, telling uPickle
 * to use a custom string as the key for that class/field rather than the
 * default string which is the full-name of that class/field.
 */
class key(s: String) extends StaticAnnotation
trait DeriveApi[M[_]]{
  val c: Context
  import c.universe._
  def typeclass: c.WeakTypeTag[M[_]]
  def typeclassName: String
  def wrapObject(t: Tree): Tree
  def wrapCase0(t: Tree, targetType: c.Type): Tree
  def wrapCase1(t: Tree, arg: String, default: Tree, typeArgs: Seq[c.Type], argTypes: Type, targetType: c.Type): Tree
  def wrapCaseN(t: Tree, args: Seq[String], defaults: Seq[Tree], typeArgs: Seq[c.Type], argTypes: Seq[Type],targetType: c.Type): Tree
  def knot(t: Tree): Tree
  def mergeTrait(ts: Seq[Tree], targetType: c.Type): Tree
}
abstract class Derive[M[_]] extends DeriveApi[M]{

  import c.universe._
  import compat._
  def typeclassFor(t: Type) = {
//    println("typeclassFor " + weakTypeOf[M[_]](typeclass))

    weakTypeOf[M[_]](typeclass) match {
      case TypeRef(a, b, _) =>
        TypeRef(a, b, List(t))
      case x =>
        println("???")
        println(x)
        println(x.getClass)
        ???
    }
  }
  def freshName = c.fresh[TermName]("upickle")


  def checkType(tpe: Type) = {
    if (tpe == typeOf[Nothing]){
      c.echo(c.enclosingPosition, "Inferred `Reader[Nothing]`, something probably went wrong")
    }
//    if (rw.banScala && tpe.typeSymbol.fullName.startsWith("scala."))
//      c.abort(c.enclosingPosition, s"this may be an error, can not generate Reader[$tpe <: ${tpe.typeSymbol.fullName}]")

  }
  def derive[T: c.WeakTypeTag] = {
    val tpe = weakTypeTag[T].tpe
    checkType(tpe)
    picklerFor(tpe)
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
      for(subtypeSym <- tpe.typeSymbol.asClass.knownDirectSubclasses) yield {
        val st = subtypeSym.asType.toType
        val baseClsArgs = st.baseType(tpe.typeSymbol).asInstanceOf[TypeRef].args
        val sub2 = st.substituteTypes(baseClsArgs.map(_.typeSymbol), tpe.args)
        //        println(Console.YELLOW + "sub2 " + Console.RESET + sub2)
        sub2
      }
    impl
    impl
  }

  /**
   * Generates a pickler for a particular type
   */
  def picklerFor(tpe: c.Type): c.Tree = {
//    println(Console.CYAN + "picklerFor " + Console.RESET + tpe)

    def implicited(tpe: Type) = q"implicitly[${typeclassFor(tpe)}]"

    c.typeCheck(implicited(tpe), withMacrosDisabled = true, silent = true) match {
      case EmptyTree =>

        val memo = collection.mutable.Map.empty[TypeKey, Map[TypeKey, TermName]]
        case class TypeKey(t: c.Type) {
          override def equals(o: Any) = t =:= o.asInstanceOf[TypeKey].t
        }
        val seen = collection.mutable.Set.empty[TypeKey]
        def rec(tpe: c.Type, name: TermName = freshName): Map[TypeKey, TermName] = {
//          println("REC " + tpe)
          val key = TypeKey(tpe)
          //                println("rec " + tpe + " " + seen)
          if (seen(TypeKey(tpe))) Map()
          else {
            memo.getOrElseUpdate(TypeKey(tpe), {

              // If it can't find any non-macro implicits, try to recurse into the type
              val dummies = tpe match {
                case TypeRef(_, _, args) =>
                  args.map(TypeKey)
                    .distinct
                    .map(_.t)
                    .map(tpe => q"implicit def $freshName: ${typeclassFor(tpe)} = ???")
                case _ => Seq.empty[Tree]
              }
              val probe = q"{..$dummies; ${implicited(tpe)}}"
              println("TC " + name + " " + probe)
              c.typeCheck(probe, withMacrosDisabled = true, silent = true) match {
                case EmptyTree =>
                  println("Empty")
                  seen.add(key)
                  tpe.normalize match {
                    case TypeRef(_, cls, args) if cls == definitions.RepeatedParamClass =>
                      rec(args(0))
                    case TypeRef(pref, cls, args)
                      if tpe.typeSymbol.isClass
                        && tpe.typeSymbol.asClass.isTrait =>

                      val subTypes = fleshedOutSubtypes(tpe.asInstanceOf[TypeRef])

                      val lol =
                        Map(key -> name) ++
                          subTypes.flatMap(rec(_)) ++
                          args.flatMap(rec(_))
                      lol
                    case TypeRef(_, cls, args) if tpe.typeSymbol.isModuleClass =>
                      Map(key -> name)
                    case TypeRef(_, cls, args) =>
                      val (companion, typeParams, argSyms) = getArgSyms(tpe)
                      val x =
                        argSyms
                          .map(_.typeSignature.substituteTypes(typeParams, args))
                          .flatMap(rec(_))
                          .toSet

                      Map(key -> name) ++ x
                    case x =>
                      Map(key -> name)
                  }

                case t =>
                  println("Present")
                  Map()
              }

            })
          }
        }

        //    println("a")
        val first = freshName
        //    println("b")
        val recTypes = try rec(tpe, first) catch {
          case e => e.printStackTrace(); throw e
        }
        //    println("c")
        //        println("recTypes " + recTypes)

        val things = recTypes.map { case (TypeKey(tpe), name) =>
          val pick =
            if (tpe.typeSymbol.asClass.isTrait) deriveTrait(tpe)
            else if (tpe.typeSymbol.isModuleClass) deriveObject(tpe)
            else deriveClass(tpe)

          q"""
            implicit lazy val $name: ${typeclassFor(tpe)} = ${knot(pick)}
          """

        }


        val returnName = freshName
        // Do this weird immediately-called-method dance to avoid weird crash
        // in 2.11.x:
        //
        // """
        // symbol variable bitmap$0 does not exist in upickle.X.<init>
        // scala.reflect.internal.FatalError: symbol variable bitmap$0 does not exist in upickle.X.<init>
        // """
        val res = q"""{
        def $returnName = {
          ..$things

          ${recTypes(TypeKey(tpe))}
        }
        $returnName
      }"""
        //    println("RES " + res)
        res
      case t => t
    }
  }

  def deriveTrait(tpe: c.Type): c.universe.Tree = {
    val clsSymbol = tpe.typeSymbol.asClass

    if (!clsSymbol.isSealed) {
      val msg = s"[error] The referenced trait [[${clsSymbol.name}]] must be sealed."
      Console.err.println(msg)
      c.abort(c.enclosingPosition, msg) /* TODO Does not show message. */
    }

    if (clsSymbol.knownDirectSubclasses.isEmpty) {
      val msg = s"The referenced trait [[${clsSymbol.name}]] does not have any sub-classes. This may " +
        "happen due to a limitation of scalac (SI-7046) given that the trait is " +
        "not in the same package. If this is the case, the hierarchy may be " +
        "defined using integer constants."
      Console.err.println(msg)
      c.abort(c.enclosingPosition, msg) /* TODO Does not show message. */
    }

    //    println("pickleTrait")
    val subPicklers =
      fleshedOutSubtypes(tpe.asInstanceOf[TypeRef])
        .map(subCls => q"implicitly[${typeclassFor(subCls)}]")
        .toSeq
    //    println(Console.GREEN + "subPicklers " + Console.RESET + subPicklerss)
    mergeTrait(subPicklers, tpe)

  }

  def deriveClass(tpe: c.Type) = {

    val (companion, paramTypes, argSyms) = getArgSyms(tpe)

    //    println("argSyms " + argSyms.map(_.typeSignature))
    val args = argSyms.map{ p =>
      customKey(p).getOrElse(p.name.toString)
    }

    val defaults = argSyms.zipWithIndex.map { case (s, i) =>
      val defaultName = newTermName("apply$default$" + (i + 1))
      companion.tpe.member(defaultName) match{
        case NoSymbol => q"null"
        case _ => q"${c.prefix}.writeJs($companion.$defaultName)"
      }
    }

    val typeArgs = tpe match {
      case TypeRef(_, _, args) => args
      case _ => c.abort(
        c.enclosingPosition,
        s"Don't know how to pickle type $tpe"
      )
    }


    def func(t: Type) = {

      if (argSyms.length == 0) t
      else {
        val base = argSyms.map(_.typeSignature.typeSymbol)
        val concrete = tpe.normalize.asInstanceOf[TypeRef].args
        if (t.typeSymbol != definitions.RepeatedParamClass) t.substituteTypes(base, concrete)
        else {
          val TypeRef(pref, sym, args) = typeOf[Seq[Int]]
          import compat._
          TypeRef(pref, sym, t.asInstanceOf[TypeRef].args)
        }
      }
    }
    val pickler =
      if (args.length == 0) // 0-arg case classes are treated like `object`s
        wrapCase0(companion, tpe)
      else if (args.length == 1) // 1-arg case classes often need their output wrapped in a Tuple1
        wrapCase1(companion, args(0), defaults(0), typeArgs, func(argSyms(0).typeSignature), tpe)
      else // Otherwise, reading and writing are kinda identical
        wrapCaseN(companion, args, defaults, typeArgs, argSyms.map(_.typeSignature).map(func), tpe)

    annotate(tpe)(q"$pickler")
  }


  def deriveObject(tpe: c.Type) = {
    val mod = tpe.typeSymbol.asClass.module
    val symTab = c.universe.asInstanceOf[reflect.internal.SymbolTable]
    val pre = tpe.asInstanceOf[symTab.Type].prefix.asInstanceOf[Type]
    val mod2 = c.universe.treeBuild.mkAttributedRef(pre, mod)

    annotate(tpe)(wrapObject(mod2))

  }

  /** If there is a sealed base class, annotate the pickled tree in the JSON
    * representation with a class label.
    */
  def annotate(tpe: c.Type)
              (pickler: c.universe.Tree) = {
    val sealedParent = tpe.baseClasses.find(_.asClass.isSealed)
    sealedParent.fold(pickler) { parent =>
      val index = customKey(tpe.typeSymbol).getOrElse(tpe.typeSymbol.fullName)
      q"${c.prefix}.annotate($pickler, $index)"
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
    val companion = companionTree(tpe)
    //    println("companionTree " + companion)
    val apply =
      companion.tpe
        .member(newTermName("apply"))
    if (apply == NoSymbol){
      c.abort(
        c.enclosingPosition,
        s"Don't know how to pickle $tpe; it's companion has no `apply` method"
      )
    }

    val argSyms =
      apply.asMethod
        .paramss
        .flatten

    (companion, apply.asMethod.typeParams, argSyms)
  }

  /**
   * Special haxx0rs to get us the real companion symbol using the
   * compiler internals, which isn't possible just using the public API
   */
  def companionTree(tpe: c.Type) = {
    val companionSymbol = tpe.typeSymbol.companionSymbol

    if (companionSymbol == NoSymbol && tpe.typeSymbol.isClass) {
      val clsSymbol = tpe.typeSymbol.asClass
      val msg = "[error] The companion symbol could not be determined for " +
        s"[[${clsSymbol.name}]]. This may be due to a bug in scalac (SI-7567) " +
        "that arises when a case class within a function is pickled. As a " +
        "workaround, move the declaration to the module-level."
      Console.err.println(msg)
      c.abort(c.enclosingPosition, msg) /* TODO Does not show message. */
    }

    val symTab = c.universe.asInstanceOf[reflect.internal.SymbolTable]
    val pre = tpe.asInstanceOf[symTab.Type].prefix.asInstanceOf[Type]
    c.universe.treeBuild.mkAttributedRef(pre, companionSymbol)
  }
}
