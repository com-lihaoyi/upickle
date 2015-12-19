package derive

/*
 * A whole bunch of test data that can be used by client libraries to try out
 * their typeclass derivation to make sure it's doing the right thing. Contains
 * roughly the  whole range of interesting shapes of types supported by derive.
 */

object ADTs {
  case class ADT0()
  case class ADTa(i: Int)
  case class ADTb(i: Int, s: String)
  case class ADTc(i: Int, s: String, t: (Double, Double))
  case class ADTd(i: Int, s: String, t: (Double, Double), a: ADTa)
  case class ADTe(i: Int, s: String, t: (Double, Double), a: ADTa, q: Seq[Double])
  case class ADTf(i: Int, s: String, t: (Double, Double), a: ADTa, q: Seq[Double], o: Option[Option[Boolean]])
  case class ADTz(t1: Int, t2: String,
                  t3: Int, t4: String,
                  t5: Int, t6: String,
                  t7: Int, t8: String,
                  t9: Int, t10: String,
                  t11: Int, t12: String,
                  t13: Int, t14: String,
                  t15: Int, t16: String,
                  t17: Int, t18: String
                   )
}
object Hierarchy {
  sealed trait A
  case class B(i: Int) extends A
  case class C(s1: String, s2: String) extends A

  sealed trait Z //new line
  case object AnZ extends Z //new line
}
object DeepHierarchy {
  sealed abstract class A
  case class B(i: Int) extends A

  sealed trait C extends A
  case class D(s: String) extends C
  case class E(b: Boolean) extends C

  sealed trait Q //new line
  case class AnQ(i: Int) extends Q //new line

  case class F(q: Q) extends C //new line


}
object Singletons{
  sealed trait AA
  case object BB extends AA
  case object CC extends AA

  case object Standalone
}
object Generic{
  case class A[T](t: T)
  case class ADT[A, B, C, D, E, F](a: A, b: B, c: C, d: D, e: E, f: F)
}
object Recursive{
  sealed trait LL
  case object End  extends LL
  case class Node(c: Int, next: LL) extends LL

  case class IntTree(value: Int, children: List[IntTree])

  sealed trait SingleTree
  case class SingleNode(value: Int, children: List[SingleTree]) extends SingleTree
}
object Annotated {
  sealed trait A
  @key("0") case class B(@key("omg") i: Int) extends A
  @key("1") case class C(@key("lol") s1: String, @key("wtf") s2: String) extends A
}
object Defaults {
  case class ADTa(i: Int = 0)
  case class ADTb(i: Int = 1, s: String)
  case class ADTc(i: Int = 2, s: String, t: (Double, Double) = (1, 2))
}
trait MixedIn{
  trait Trt1{
    case class ClsA(s: String)
  }
  trait Trt2 extends Trt1{
    case class ClsB(i: Int)
  }
  object Obj extends Trt2
}

object MixedIn extends MixedIn


object Varargs{
  case class Sentence(a: String, bs: String*)
}
object Covariant{
  case class Tree[+T](value: T)
}

object Exponential{
  case class A1 (x: A2 , y: A2 )
  case class A2 (x: A3 , y: A3 )
  case class A3 (x: A4 , y: A4 )
  case class A4 (x: A5 , y: A5 )
  case class A5 (x: A6 , y: A6 )
  case class A6 (x: A7 , y: A7 )
  case class A7 (x: A8 , y: A8 )
  case class A8 (x: A9 , y: A9 )
  case class A9 (x: A10, y: A10)
  case class A10(x: A11, y: A11)
  case class A11(x: A12, y: A12)
  case class A12(x: A13, y: A13)
  case class A13(x: A14, y: A14)
  case class A14(x: A15, y: A15)
  case class A15(x: A16, y: A16)
  case class A16(x: A17, y: A17)
  case class A17(x: A18, y: A18)
  case class A18()
}

object GenericADTs{
  sealed trait Small[A]
  case class Small1[A](key: A) extends Small[A]

  sealed trait Delta[+A, +B]
  object Delta {
    case class Insert[A, B](key: A, value: B) extends Delta[A, B]
    case class Remove[A](key: A) extends Delta[A, Nothing]
    case class Clear() extends Delta[Nothing, Nothing]
  }
  sealed trait DeltaInvariant[A, B]
  object DeltaInvariant {
    case class Insert[A, B](key: A, value: B) extends DeltaInvariant[A, B]
    case class Remove[A, B](key: A) extends DeltaInvariant[A, B]
    case class Clear[A, B]() extends DeltaInvariant[A, B]
  }
  sealed trait DeltaHardcoded[A, B]
  object DeltaHardcoded {
    case class Insert[A, B](key: A, value: B) extends DeltaHardcoded[A, B]
    case class Remove[A](key: A) extends DeltaHardcoded[A, String]
    case class Clear() extends DeltaHardcoded[Seq[Int], String]
  }
}
object Amorphous{
  class A()
  class B(i: Int){
    val x = "lol"
  }
}
// issue #95
// For some reason this stuff must live top-level; the test fails to
// go red when the case classes are moved inside a wrapper object even
// when the fix is backed out
case class C1(name : String, types : List[String])
case class C2(results : List[C1])

case class Result2(name : String,
                   whatever : String,
                   types : List[String]
                    )

case class GeoCoding2(results : List[Result2], status: String)

object Issue94{
  class Foo(val x: String){
    override def toString = x
    override def hashCode = x.hashCode
    override def equals(o: Any) = o match{
      case f: Foo => x == f.x
      case _ => false
    }
  }

  case class Example(ids: List[Foo])
  case class Example2(ids: List[List[Foo]])

}
object Issue92{
  abstract class Rational extends Ordered[Rational]
}
object Issue96{
  sealed trait Trait
  class BadApply(i: Int) extends Trait
  object BadApply{
    def apply(i: Int) = new BadApply(i)
    def apply(i: Int, s: String) = new BadApply(i + s.toInt)
  }
  sealed trait Field { }

  case class ChoiceField(choices: Array[String]) extends Field
}

sealed trait Ast{
  def offset: Int
}

/**
 * Sample AST taken from the Scalatex project
 *
 * https://github.com/lihaoyi/Scalatex/
 *
 * It's a use case where each case class inherits from multiple distinct
 * sealed traits, which aren't a strict hierarchy
 */
object Ast{

  /**
   * @param parts The various bits of text and other things which make up this block
   * @param offset
   */
  case class Block(offset: Int, parts: Seq[Block.Sub]) extends Chain.Sub with Block.Sub
  object Block{
    sealed trait Sub extends Ast
    case class Text(offset: Int, txt: String) extends Block.Sub
    case class For(offset: Int, generators: String, block: Block) extends Block.Sub
    case class IfElse(offset: Int, condition: String, block: Block, elseBlock: Option[Block]) extends Block.Sub
  }
  case class Header(offset: Int, front: String, block: Block) extends Block.Sub with Chain.Sub

  /**
   * @param lhs The first expression in this method-chain
   * @param parts A list of follow-on items chained to the first
   * @param offset
   */
  case class Chain(offset: Int, lhs: String, parts: Seq[Chain.Sub]) extends Block.Sub
  object Chain{
    sealed trait Sub extends Ast
    case class Prop(offset: Int, str: String) extends Sub
    case class TypeArgs(offset: Int, str: String) extends Sub
    case class Args(offset: Int, str: String) extends Sub
  }
}


/**
 * Sample AST from the FastParse PythonParse project. Doesn't work yet
 *
 * A python abstract syntax tree
 *
 * Basically transcribed from https://docs.python.org/2/library/ast.html
 */
object PythonAst{

  sealed trait expr
  object expr{
    case class BoolOp(op: Int, values: expr) extends expr
    case class Yield(value: Option[expr]) extends expr
    case class Compare(left: expr, comparators: Seq[expr]) extends expr
    case class Call(func: expr, starargs: Option[expr]) extends expr
    case class List(elts: Seq[expr], ctx: expr_context) extends expr
  }

  sealed trait expr_context
  object expr_context{

    case object Load extends expr_context

  }

}