package upickle

import ScalaVersionStubs._
import upickle.Macros.RW
import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import compat._
import acyclic.file
/**
 * Used to annotate either case classes or their fields, telling uPickle
 * to use a custom string as the key for that class/field rather than the
 * default string which is the full-name of that class/field.
 */
class key(s: String) extends StaticAnnotation

/**
 * Implementation of macros used by uPickle to serialize and deserialize
 * case classes automatically. You probably shouldn't need to use these
 * directly, since they are called implicitly when trying to read/write
 * types you don't have a Reader/Writer in scope for.
 */
object Macros {
  class RW(val short: String, val long: String, val actionNames: Seq[String])

  object RW {

    object R extends RW("R", "Reader", Seq("apply"))

    object W extends RW("W", "Writer", Seq("unapply", "unapplySeq"))

  }

  def macroRImpl[T, R[_]](c0: Context)(implicit e1: c0.WeakTypeTag[T], e2: c0.WeakTypeTag[R[_]]): c0.Expr[R[T]] = {
    new Macros[R](RW.R){val c: c0.type = c0}.read[T](implicitly[c0.WeakTypeTag[T]])
  }

  def macroWImpl[T, W[_]](c0: Context)(implicit e1: c0.WeakTypeTag[T], e2: c0.WeakTypeTag[W[_]]): c0.Expr[W[T]] = {
    new Macros[W](RW.W){val c: c0.type = c0}.write[T](implicitly[c0.WeakTypeTag[T]])
  }
}

abstract class Macros[M[_]](rw: RW){
  val c: Context
  import Macros._
  import c.universe._
  def internal = q"${c.prefix}.Internal"
  def freshName = c.fresh[TermName]("upickle")
  def checkType(tpe: Type) = {
    if (tpe == typeOf[Nothing]){
      c.echo(c.enclosingPosition, "Inferred `Reader[Nothing]`, something probably went wrong")
    }
    if (tpe.typeSymbol.fullName.startsWith("scala."))
      c.abort(c.enclosingPosition, s"this may be an error, can not generate Reader[$tpe <: ${tpe.typeSymbol.fullName}]")

  }
  def read[T: c.WeakTypeTag] = {
    val tpe = weakTypeTag[T].tpe
    //    println(Console.BLUE + "START " + Console.RESET + c.enclosingPosition)
    checkType(tpe)
    val res = c.Expr[M[T]] {
      picklerFor(tpe)(
        _.map(p => q"$p.read": Tree)
          .reduce((a, b) => q"$a orElse $b")
      )
    }
    //    cas.callsiteTyper.context1.openImplicits = o//pen
    //    println(Console.BLUE + "END " + Console.RESET + tpe)
//        println(Console.BLUE + res + Console.RESET + tpe)
    res
  }

  def write[T: c.WeakTypeTag] = {
    val tpe = weakTypeTag[T].tpe
//    println(Console.BLUE + "START " + Console.RESET + tpe)
    checkType(tpe)
//        println(Console.CYAN + "WRITE" + Console.RESET)


    val res = picklerFor(tpe) { things =>
      if (things.length == 1) q"$internal.merge0(${things(0)}.write)"
      else things.map(p => q"$p.write": Tree)
        .reduce((a, b) => q"$internal.merge($a, $b)")
    }
    //    println(Console.GREEN + "TYPECHECK" + Console.RESET)
//            println(res)
//        println(Console.BLUE + "END " + Console.RESET )
    c.Expr[M[T]](res)
  }

  def fleshedOutSubtypes(tpe: TypeRef) = {
    //    println(Console.CYAN + "fleshedOutSubTypes " + Console.RESET + tpe)
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
   *
   * @param tpe The type we are generating the pickler for
   * @param rw Configuration that determines whether it's a Reader or
   *           a Writer, together with the various names which vary
   *           between those two choices
   * @param treeMaker How to merge the trees of the multiple subpicklers
   *                  into one larger tree
   */
  def picklerFor(tpe: c.Type)
                (treeMaker: Seq[c.Tree] => c.Tree): c.Tree = {
    //    println(Console.CYAN + "picklerFor " + Console.RESET + tpe)

    def implicited(tpe: Type) = q"implicitly[${c.prefix}.${newTypeName(rw.long)}[$tpe]]"
    c.typeCheck(implicited(tpe), withMacrosDisabled = true, silent = true) match {
      case EmptyTree =>

        val memo = collection.mutable.Map.empty[TypeKey, Map[TypeKey, TermName]]
        case class TypeKey(t: c.Type) {
          override def equals(o: Any) = t =:= o.asInstanceOf[TypeKey].t
        }
        val seen = collection.mutable.Set.empty[TypeKey]
        def rec(tpe: c.Type, name: TermName = freshName): Map[TypeKey, TermName] = {
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
                    .map(tpe => q"implicit def ${freshName}: ${c.prefix}.${newTypeName(rw.long)}[${tpe}] = ???")
                case _ => Seq.empty[Tree]
              }
              val probe = q"{..$dummies; ${implicited(tpe)}}"
//              println("TC " + name + " " + probe)
              c.typeCheck(probe, withMacrosDisabled = true, silent = true) match {
                case EmptyTree =>
//                  println("Empty")
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
//                  println("Present " + t)
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
        val knotName = newTermName("knot" + rw.short)
//        println("recTypes " + recTypes)

        val things = recTypes.map { case (TypeKey(tpe), name) =>
          val pick =
            if (tpe.typeSymbol.asClass.isTrait) pickleTrait(tpe)(treeMaker)
            else if (tpe.typeSymbol.isModuleClass) pickleCaseObject(tpe)(treeMaker)
            else pickleClass(tpe)(treeMaker)
          val i = freshName
          val x = freshName
          val tree = q"""
          implicit lazy val $name: ${c.prefix}.${newTypeName(rw.long)}[$tpe] = {
            new ${c.prefix}.Knot.${newTypeName(rw.short)}[$tpe]($pick)
          }
        """
          (i, tree)
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
          ..${things.map(_._2)}

          ${recTypes(TypeKey(tpe))}
        }
        $returnName
      }"""
        //    println("RES " + res)
        res
      case t => t
    }
  }

  def pickleTrait(tpe: c.Type)
                 (treeMaker: Seq[c.Tree] => c.Tree): c.universe.Tree = {
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
        .map(subCls => q"implicitly[${c.prefix}.${newTypeName(rw.long)}[$subCls]]")
        .toSeq
    //    println(Console.GREEN + "subPicklers " + Console.RESET + subPicklerss)
    val combined = treeMaker(subPicklers)

    q"""${c.prefix}.${newTermName(rw.long)}[$tpe]($combined)"""
  }

  def pickleCaseObject(tpe: c.Type)
                      (treeMaker: Seq[c.Tree] => c.Tree) = {
    val mod = tpe.typeSymbol.asClass.module
    val symTab = c.universe.asInstanceOf[reflect.internal.SymbolTable]
    val pre = tpe.asInstanceOf[symTab.Type].prefix.asInstanceOf[Type]
    val mod2 = c.universe.treeBuild.mkAttributedRef(pre, mod)
    annotate(tpe)(q"$internal.${newTermName("Case0"+rw.short)}[$tpe]($mod2)")
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
  def pickleClass(tpe: c.Type)(treeMaker: Seq[c.Tree] => c.Tree) = {

    val (companion, _, argSyms) = getArgSyms(tpe)

    //    println("argSyms " + argSyms.map(_.typeSignature))
    val args = argSyms.map{ p =>
      customKey(p).getOrElse(p.name.toString)
    }

    val rwName = newTermName(s"Case${args.length}${rw.short}")

    val actionName = rw.actionNames
      .map(newTermName(_))
      .find(companion.tpe.member(_) != NoSymbol)
      .getOrElse(c.abort(c.enclosingPosition, "None of the following methods " +
      "were defined: " + rw.actionNames.mkString(" ")))

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

    val pickler =
      if (args.length == 0) // 0-arg case classes are treated like `object`s
        q"$internal.${newTermName("Case0"+rw.short)}($companion())"
      else if (args.length == 1 && rw == RW.W) // 1-arg case classes need their output wrapped in a Tuple1
        q"$internal.$rwName($companion.$actionName[..$typeArgs](_: $tpe).map(Tuple1.apply), Array(..$args), Array(..$defaults)): ${c.prefix}.${newTypeName(rw.long)}[$tpe]"
      else // Otherwise, reading and writing are kinda identical
        q"$internal.$rwName($companion.$actionName[..$typeArgs], Array(..$args), Array(..$defaults)): ${c.prefix}.${newTypeName(rw.long)}[$tpe]"

    annotate(tpe)(pickler)
  }


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

