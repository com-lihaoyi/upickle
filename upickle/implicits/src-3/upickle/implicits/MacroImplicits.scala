package upickle.implicits

import deriving.Mirror
import scala.reflect.ClassTag
import scala.compiletime.erasedValue

trait MacroImplicits extends Readers with Writers with upickle.core.Annotator:
  this: upickle.core.Types =>

  inline def macroRW[T: ClassTag](using Mirror.Of[T]): ReadWriter[T] =
    ReadWriter.join(macroR[T], macroW[T])

  inline def macroRWAll[T: ClassTag](using Mirror.Of[T]): ReadWriter[T] =
    ReadWriter.join(macroRAll[T], macroWAll[T])


  // Usually, we would use an extension method to add `derived` to ReadWriter's
  // companion object. Something along the lines of:
  //
  //   extension [T](r: ReadWriter.type)
  //     inline def derived(using Mirror.Of[T]): ReadWriter[T] = macroRW[T]
  //
  // Unfortunately however, the above does not work for typeclass derivation.
  // Consider the following:
  //
  //   case class Foo() derives ReadWriter
  //
  // which is syntax sugar for:
  //
  //   object Foo:
  //     given ReadWriter[Foo] = ReadWriter.derived
  //
  // Now, since the type parameter of the extension must come after `extension`
  // and is not allowed to be part of the method itself, the compiler cannot
  // infer the correct type, and hence the extension lookup fails.
  //
  // As is mentioned here, https://dotty.epfl.ch/docs/reference/contextual/extension-methods.html#generic-extensions,
  // this limitation may be lifted in the future:
  //
  // > Note: Type parameters have to be given after the extension keyword; they
  // > cannot be given after the def. This restriction might be lifted in the
  // > future once we support multiple type parameter clauses in a method. By
  // > contrast, using clauses can be defined for the extension as well as per
  // > def.
  //
  // Until that is the case, we'll have to resort to using Scala 2's implicit
  // classes to emulate extension methods for deriving readers and writers.
  implicit class ReadWriterExtension(r: ReadWriter.type):
    inline def derived[T](using Mirror.Of[T], ClassTag[T]): ReadWriter[T] = macroRWAll[T]
  end ReadWriterExtension

end MacroImplicits
