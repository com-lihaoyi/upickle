package upickle

import ScalaVersionStubs._
import scala.annotation.StaticAnnotation
import scala.language.experimental.macros

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

  def macroRImpl[T: c.WeakTypeTag](c: Context) = {
    import c.universe._
    val tpe = weakTypeTag[T].tpe

    assert(!tpe.typeSymbol.fullName.startsWith("scala."))

    c.Expr[Reader[T]] {
      val x = picklerFor(c)(tpe, RW.R)(
        _.map(p => q"$p.read": Tree)
         .reduce((a, b) => q"$a orElse $b")
      )

      val msg = "Tagged Object " + tpe.typeSymbol.fullName
      q"""upickle.Internal.validateReader($msg){$x}"""
    }
  }

  def macroWImpl[T: c.WeakTypeTag](c: Context) = {
    import c.universe._
    val tpe = weakTypeTag[T].tpe

    assert(!tpe.typeSymbol.fullName.startsWith("scala."))

    c.Expr[Writer[T]] {
      picklerFor(c)(tpe, RW.W) { things =>
        if (things.length == 1) q"upickle.Internal.merge0(${things(0)}.write)"
        else things.map(p => q"$p.write": Tree)
                   .reduce((a, b) => q"upickle.Internal.merge($a, $b)")
      }
    }
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
  def picklerFor(c: Context)
                (tpe: c.Type, rw: RW)
                (treeMaker: Seq[c.Tree] => c.Tree): c.Tree =
  {
    val pick =
      if (tpe.typeSymbol.asClass.isTrait) pickleTrait(c)(tpe, rw)(treeMaker)
      else if (tpe.typeSymbol.isModuleClass) pickleCaseObject(c)(tpe, rw)(treeMaker)
      else pickleClass(c)(tpe, rw)(treeMaker)

    import c.universe._

    val knotName = newTermName("knot" + rw.short)

    val i = c.fresh[TermName]("i")
    val x = c.fresh[TermName]("x")

    q"""
       upickle.Internal.$knotName{implicit $i: upickle.Knot.${newTypeName(rw.short)}[$tpe] =>
          val $x = $pick
          $i.copyFrom($x)
          $x
        }
     """
  }

  def pickleTrait(c: Context)
                 (tpe: c.Type, rw: RW)
                 (treeMaker: Seq[c.Tree] => c.Tree): c.universe.Tree =
  {
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

    val subPicklers =
      clsSymbol.knownDirectSubclasses.map(subCls =>
        picklerFor(c)(subCls.asType.toType, rw)(treeMaker)
      ).toSeq

    val combined = treeMaker(subPicklers)

    import c.universe._
    q"""upickle.${newTermName(rw.long)}[$tpe]($combined)"""
  }

  def pickleCaseObject(c: Context)
                      (tpe: c.Type, rw: RW)
                      (treeMaker: Seq[c.Tree] => c.Tree) =
  {
    val mod = tpe.typeSymbol.asClass.module

    import c.universe._
    annotate(c)(tpe)(q"upickle.Internal.${newTermName("Case0"+rw.short)}[$tpe]($mod)")
  }

  /** If there is a sealed base class, annotate the pickled tree in the JSON
    * representation with a class label.
    */
  def annotate(c: Context)
              (tpe: c.Type)
              (pickler: c.universe.Tree) =
  {
    import c.universe._
    val sealedParent = tpe.baseClasses.find(_.asClass.isSealed)
    sealedParent.fold(pickler) { parent =>
      val index = customKey(c)(tpe.typeSymbol).getOrElse(tpe.typeSymbol.fullName)
      q"upickle.Internal.annotate($pickler, $index)"
    }
  }

  /**
   * Get the custom @key annotation from the parameter Symbol if it exists
   */
  def customKey(c: Context)(sym: c.Symbol): Option[String] = {
    import c.universe._
    sym.annotations
      .find(_.tpe == typeOf[key])
      .flatMap(_.scalaArgs.headOption)
      .map{case Literal(Constant(s)) => s.toString}
  }

  def pickleClass(c: Context)
                 (tpe: c.Type, rw: RW)
                 (treeMaker: Seq[c.Tree] => c.Tree) =
  {
    import c.universe._

    val companion = companionTree(c)(tpe)

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
      tpe.member(newTermName("<init>"))
         .typeSignatureIn(tpe) match {case MethodType(params, result) => params }

    val argSymTypes = argSyms.map(_.typeSignature).map{
      case TypeRef(a, b, c)  if b.toString == "class <repeated>" =>
        typeOf[Seq[String]] match{
          case TypeRef(_, b2, _) => TypeRef(a, b2, c)
        }
      case t => t
    }

    val args = argSyms.map { p =>
      customKey(c)(p).getOrElse(p.name.toString)
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
        case _ => q"upickle.writeJs($companion.$defaultName)"
      }
    }

    val typeArgs = tpe match {
      case TypeRef(_, _, args) => args
      case _ => c.abort(
        c.enclosingPosition,
        s"Don't know how to pickle type $tpe"
      )
    }

    val (typeArgNames, typeArgAssigns) = argSymTypes.map{t =>

//      if (t.typeSignature.toString == "")
      val name = newTermName(c.fresh())
      val tpeTree = tq"${newTypeName(rw.long)}[$t]"
      (name, Seq(q"val $name = implicitly[$tpeTree]", q"implicit val ${newTermName(c.fresh())} = $name"))
    }.unzip

    val pickler =
      if (args.length == 0) // 0-arg case classes are treated like `object`s
        q"upickle.Internal.${newTermName("Case0"+rw.short)}($companion())"
      else if (args.length == 1 && rw == RW.W) // 1-arg case classes need their output wrapped in a Tuple1
        q"upickle.Internal.$rwName(x => $companion.$actionName[..$typeArgs](x).map(Tuple1.apply), Array(..$args), Array(..$defaults)): upickle.${newTypeName(rw.long)}[$tpe]"
      else // Otherwise, reading and writing are kinda identical
        q"""{
          ..${typeArgAssigns.flatten}

          {
            upickle.Internal.$rwName[..$argSymTypes, $tpe](
              $companion.$actionName[..$typeArgs],
              Array(..$args),
              Array(..$defaults)
            )(..$typeArgNames): upickle.${newTypeName(rw.long)}[$tpe]
          }
        }"""
    val x = annotate(c)(tpe)(pickler)
//    println(x)
    x
  }

  def companionTree(c: Context)(tpe: c.Type) = {
    val companionSymbol = tpe.typeSymbol.companionSymbol

    if (companionSymbol == c.universe.NoSymbol) {
      val clsSymbol = tpe.typeSymbol.asClass
      val msg = "[error] The companion symbol could not be determined for " +
        s"[[${clsSymbol.name}]]. This may be due to a bug in scalac (SI-7567) " +
        "that arises when a case class within a function is pickled. As a " +
        "workaround, move the declaration to the module-level."
      Console.err.println(msg)
      c.abort(c.enclosingPosition, msg) /* TODO Does not show message. */
    }

    import c.universe._
    val symTab = c.universe.asInstanceOf[reflect.internal.SymbolTable]
    val pre = tpe.asInstanceOf[symTab.Type].prefix.asInstanceOf[Type]
    c.universe.treeBuild.mkAttributedRef(pre, companionSymbol)
  }
}
 
