µPickle 0.2.2
=============

uPickle (pronounced micro-pickle) is a lightweight serialization library for Scala. It's key features are:

- Less than 1000 lines of code
- Zero-reflection 100% static serialization and deserialization
- [Human-readable JSON encoding](#getting-started), with a fast [JSON API](#json-api)
- [A large, well-defined set of supported types, with well-defined semantics](#supported-types)
- Handling of [default values](#defaults) and [custom keys](#custom-keys), for maintaining backwards compatiblity while schemas change
- Minimal dependencies: Only depends on [Jawn](https://github.com/non/jawn) on the JVM, and on the Javascript standard library in Scala.js
- [Works in ScalaJS, allowing transfer of structured data between the JVM and Javascript](#scalajs)

Getting Started
===============

Add the following to your SBT config:

```scala
resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"

libraryDependencies += "com.lihaoyi" %% "upickle" % "0.2.2"
```

And then you can immediately start writing and reading common Scala objects to strings:

```scala
import upickle._

write(1)
res1: String = 1

write(Seq(1, 2, 3))
res2: String = [1, 2, 3]

read[Seq[Int]]("[1, 2, 3]")
res3: Seq[Int] = List(1, 2, 3)

write((1, "omg", true))
res4: String = [1, "omg", true]

read[(Int, String, Boolean)]("""[1, "omg", true]""")
res5: (Int, String, Boolean) = (1,omg,true)
```

ScalaJS
=======

For ScalaJS applications, use this dependencies instead:

```scala
libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.2.2"
```

Other than that, everything is used the same way. upickle-0.2.2 is only compatible with ScalaJS 0.5.3+.

Supported Types
===============

Out of the box, uPickle supports writing and reading the following types:

- `Boolean`, `Byte`, `Char`, `Short`, `Int`, `Long`, `Float`, `Double`
- `Tuple`s from 1 to 22
- Immutable `Seq`, `List`, `Vector`, `Set`, `SortedSet`, `Option`, `Array`, `Map`s
- `Duration`, `Either`
- Stand-alone `case class`es and `case object`s, and their generic equivalents,
- Non-generic `case class`es and `case object`s that are part of a `sealed trait` or `sealed class` hierarchy
- `sealed trait` and `sealed class`es themselves, assuming that all subclasses are picklable

Readability/writability is recursive: a container such as a `Tuple` or `case class` is only readable if all its contents are readable, and only writable if all its contents are writable. That means that you cannot serialize a `List[Any]`, since uPickle doesn't provide a generic way of serializing `Any`. Case classes are only serializable up to 22 fields.

Case classes are serialized using the `apply` and `unapply` methods on their companion objects. This means that you can make your own classes serializable by giving them companions `apply` and `unapply`. `sealed` hierarchies are serialized as tagged unions: whatever the serialization of the actual object, together with the fully-qualified name of its class, so the correct class in the sealed hierarchy can be reconstituted later.   

That concludes the list of supported types. Anything else is not supported.

Default Picklers
================

This is a non-comprehensive list of what the most commonly-used types pickle to using uPickle. To begin, let's import upickle

```scala
import upickle._
```

Booleans are serialized as JSON booleans

```scala
write(true: Boolean)             // res0: String = true
```

Numbers are serialized as JSON numbers
 
```scala
write(12: Int)                   // res1: String = 12
write(12: Short)                 // res2: String = 12
write(12: Byte)                  // res3: String = 12

write(12.3f: Float)              // res6: String = 12.3
write(12.3: Double)              // res7: String = 12.3
```

Except for `Long`s, which too large for Javascript. These are serialized as JSON Strings, keeping the interchange format compatible with the browser's own JSON parser, which provides the best performance in Scala.js  

```scala
write(12: Long)                  // res4: String = "12"
write(4000000000000L: Long)      // res8: String = "4000000000000"
```

Special values of `Double`s and `Float`s are also serialized as Strings

```scala
write(1.0/0: Double)             // res9: String = "Infinity"
write(Float.PositiveInfinity)    // res10: String = "Infinity"
write(Float.NegativeInfinity)    // res11: String = "-Infinity"
```

Both `Char`s and `String`s are serialized as Strings

```scala
write('o')                       // res12: String = "o"
write("omg")                     // res13: String = "omg"
```

`Array`s and most immutable collections are serialized as JSON lists

```scala
write(Array(1, 2, 3))            // res14: String = [1, 2, 3]
write(Seq(1, 2, 3))              // res15: String = [1, 2, 3]
write(Vector(1, 2, 3))           // res16: String = [1, 2, 3]
write(List(1, 2, 3))             // res17: String = [1, 2, 3]
write(SortedSet(1, 2, 3))        // res18: String = [1, 2, 3]
```

`Option`s are serialized as JSON lists with 0 or 1 element

```Scala
write(Some(1))                   // res19: String = [1]
write(None)                      // res20: String = []
```

Tuples of all sizes (1-22) are serialized as heterogenous JSON lists

```scala
write((1, "omg"))                // res21: String = [1, "omg"]
write((1, "omg", true))          // res22: String = [1, "omg", true]
```

Case classes of sizes 1-22 are serialized as JSON dictionaries with the keys being the names of each field

```scala
case class Thing(a: Int, b: String)
write(Thing(1, "gg"))           
// res23: String = {"a": 1, "b": "gg"}

case class Big(i: Int, b: Boolean, str: String, c: Char, d: Double)
write(Big(1, true, "lol", 'Z', 0.01)) 
// res24: String = {"i": 1, "b": true, "str": "lol", "c": "Z", "d": 0.01}
```

Sealed hierarchies are serialized as tagged values, the serialized object tagged with its full name:

```scala
sealed trait IntOrTuple
case class IntThing(i: Int) extends IntOrTuple
case class TupleThing(name: String, t: (Int, Int)) extends IntOrTuple

write(IntThing(1))              
// res25: String = ["IntThing", {"i": 1}]

write(TupleThing("naeem", (1, 2)))
// res26: String = ["TupleThing", {"name": "naeem", "t": [1, 2]}]
```

Serializability is recursive; you can serialize a type only if all its members are serializable. That means that collections, tuples and case-classes made only of serializable members are themselves serializable

```scala
write((((1, 2), (3, 4)), ((5, 6), (7, 8))))
// res27: String = [[[1, 2], [3, 4]], [[5, 6], [7, 8]]]

write(Seq(Thing(1, "g"), Thing(2, "k")))
// res28: String = [{"a": 1, "b": "g"}, {"a": 2, "b": "k"}]

case class Foo(i: Int)
case class Bar(name: String, foos: Seq[Foo])

write(Bar("bearrr", Seq(Foo(1), Foo(2), Foo(3))))
// res29: String = {"name": "bearrr", "foos": [{"i": 1}, {"i": 2}, {"i": 3}]}
```

Exceptions
==========

uPickle only throws exceptions on unpickling; if a pickler is properly defined, serializing a data structure to a `String` should never throw an exception.

On unpickling, uPickle throws one of two subclasses of `upickle.Invalid`:

- `upickle.Invalid.Json`: thrown when unpickling fails at the first step which attempst to convert the incoming `String` into semi-structured JSON data. The exception contains data about where parsing failed (`input`, `line`, `col`) as well as a human-readable error message.
- `upickle.Invalid.Data`: thrown when unpickling fails at the second step which attempts to convert the parsed JSON tree into structured data of the desired type. Contains the offending JSON subtree in `data`, along with a human-readable error message. 

Defaults
========

If a field is missing upon deserialization, uPickle uses the default value if one exists

```scala
import upickle._
case class Foo(i: Int = 10, s: String = "lol")

read[Foo]("{}")             // res1: Foo = Foo(10,lol)

read[Foo]("""{"i": 123}""") // res2: Foo = Foo(123,lol)
```

In addition, if a field at serialization time has the same value as the default, uPickle leaves it out of the serialized blob
  
```scala
write(Foo(i = 11, s = "lol")) // res3: String = {"i": 11}

write(Foo(i = 10, s = "lol")) // res4: String = {}

write(Foo())                  // res5: String = {}
```

This allows you to make schema changes gradually, assuming you have already pickled some data and want to add new fields to the case classes you pickled. Simply give the new fields a default value (e.g. `""` for Strings, or wrap it in an `Option[T]` and make the default `None`) and uPickle will happily read the old data, filling in the missing field using the default value.

Custom Keys
===========

uPickle allows you to specify the key that a field is serialized with via a `@key` annotation

```scala
case class Bar(@key("hehehe") kekeke: Int)

write(Bar(10))                  // res6: String = {"hehehe": 10}

read[Bar]("""{"hehehe": 10}""") // res7: Bar = Bar(10)
```

Practically, this is useful if you want to rename the field within your Scala code while still maintaining backwards compatibility with previously-pickled objects. Simple rename the field and add a `@key("...")` with the old name so uPickle can continue to work with the old objects correctly. 

You can also use `@key` to change the name used when pickling the case class itself. Normally case classes are pickled without their name, but an exception is made for members of sealed hierarchies which are tagged with their fully-qualified name. uPickle allows you to use `@key` to override what the class is tagged with:
  
```scala
sealed trait A
@key("Bee") case class B(i: Int) extends A
case object C extends A

write(B(10))                      // res9: String = ["Bee", {"i": 10}]
read[B]("""["Bee", {"i": 10}]""") // res11: B = B(10)
```

This is useful in cases where you wish to rename the class within your Scala code, or move it to a different package, but want to preserve backwards compatibility with previously pickled instances of that class.

Limitations
===========

uPickle is a work in progress, and doesn't currently support:

- Circular object graphs
- Nulls
- Reflective reading and writing
- Read/writing of untyped values e.g. `Any`
- Generic sealed hierarchies
- Read/writing arbitrarily shaped objects

Most of these limitations are inherent in the fact that ScalaJS does not support reflection, and are unlikely to ever go away. In general, uPickle is designed to serialize statically-typed, tree-shaped, immutable data structures. Anything more complex is out of scope.

JSON API
========

Although uPickle's object read/writing API makes does not expose you to it, under the hood it uses a nice JSON serialization format. Despite being less-compact than binary formats, this allows for very-fast serializing and deserializing from Strings on both Scala-JVM (which has other alternatives) and ScalaJS, where JSON is really your only choice. The JSON API is minimal but nonetheless very convenient, and can be used directly.  

uPickle bundles two very-fast JSON parsers, which it uses for parsing strings into structured-trees, before then marshalling them into typed objects.
 
- [Jawn](https://github.com/non/jawn) on the JVM
- [JSON.parse](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/parse) on Scala.js

That makes uPickle's JSON library competitive with the highest performance JSON libraries both on the JVM (GSON, Jackson, etc.) as well as in Javascript. 

uPickle's JSON API is exposed in two places: in our `upickle.Js.*` AST:

```scala
object Js {
  sealed trait Value extends Any {
    def value: Any
    def apply(i: Int): Value = this.asInstanceOf[Arr].value(i)
    def apply(s: java.lang.String): Value = this.asInstanceOf[Obj].value.find(_._1 == s).get._2
  }
  case class Str(value: java.lang.String) extends AnyVal with Value
  case class Obj(value: (java.lang.String, Value)*) extends AnyVal with Value
  case class Arr(value: Value*) extends AnyVal with Value
  case class Num(value: Double) extends AnyVal with Value
  case object False extends Value{
    def value = false
  }
  case object True extends Value{
    def value = true
  }
  case object Null extends Value{
    def value = null
  }
}
```

As well as in the `upickle.json.read` and `upickle.json.write` functions:

```scala
def read(s: String): Js.Value
def write(v: Js.Value): String
```

Which you use to convert between structured `Js.*` trees and unstructured `String`s. As described earlier, the implementation of these functions differs between ScalaJVM/ScalaJS.

uPickle does not provide any other utilities are JSON that other libraries do (zippers, lenses, combinators, ...). If you're looking for a compact JSON AST to construct or pattern match on, together with fast serializing and deserializing, it may do the trick. 

Why uPickle
===========

I wrote uPickle because I needed a transparent serialization library that worked both in Scala-JVM and Scala-JS, and my dissatisfaction with existing solutions:

- None of the libraries I could find were pure Scala: [spray-json](https://github.com/spray/spray-json) uses [parboiled1](https://github.com/sirthias/parboiled/wiki) which is written in Java, [play-json](http://www.playframework.com/documentation/2.2.x/ScalaJson) uses [Jackson](http://jackson.codehaus.org/), which uses Java/reflection. [scala-pickling](https://github.com/scala/pickling) silently falls back to reflection. This makes them difficult to port to ScalaJS, which supports neither Java nor reflection.
- Those libraries also typically have non-trivial dependency chains. That also makes them hard to port to ScalaJS, since I'd need to port all of their dependencies too.
- Lastly, many aim for a very ambitious target: untyped serialization of arbitrary object graphs. That forces them to use reflection, and makes their internals and semantics much more complex.

uPickle on the other hand aims much lower: by limiting the scope of the problem to statically-typed, tree-like, immutable data structures, it greatly simplifies both the internal implementation and the external API and behavior of the library. uPickle serializes objects using a very simple set of rules ("Does it have an implicit? Is it a class with `apply`/`unapply` on the companion?") that makes its behavior predictable and simple to understand.

Version History
===============

0.2.2
-----

- Swapped over from the hand-rolled parser to using `Jawn`/`JSON.parse` on the two platforms, resulting in a 10-15x speedup for JSON handling.
- Renamed `Js.{String, Object, Array, Number}` into `Js.{Str, Obj, Arr, Num}`, and made `Js.Arr` and `Js.Obj` use varargs, to allow for better direct-use.
- Documented and exposed JSON API for direct use by users of the library.

0.2.1
-----

- Improved error messages for unpickle-able types
- ScalaJS version now built against 0.5.3

0.2.0
-----

- Members of sealed trait/class hierarchies are now keyed with the fully-qualified name of their class, rather than an index, as it is less likely to change due to adding or removing classes
- Members of sealed hierarchies and parameters now support a `upickle.key("...")` annotation, which allows you to override the default key used (which is the class/parameter name) with a custom one, allowing you to change the class/param name in your code while maintaining compatibility with serialized structures
- Default parameters are now supported: they are used to substitute missing keys when reading, and cause the key/value pair to be omitted if the serialized value matches the default when writing
- Missing keys when deserializing case classes now throws a proper `Invalid.Data` exception
- `object`s are now serialized as `{}` rather than `[]`, better matching the style of case classes
- 0-argument case classes, previously unsupported, now serialize to `{}` the same way as `object`s
- Fixed a bug that was preventing multi-level sealed class hierarchies from being serialized due to a compilation error
- Fixed a bug causing case classes nested in other packages/objects and referred to by their qualified paths to fail pickling
- Tightened up error handling semantics, swapping out several `MatchError`s with `Invalid.Data` errors

0.1.7
-----

- Cleaned up the external API, marking lots of things which should have been private private or stuffing them in the `Internals` namespace
- Organized things such that only a single import `import upickle._` is necessary to use the library

0.1.6
-----

- Tuples and case classes now have implicit picklers up to an arity limit of 22.
- Case classes now serialize as JSON dictionaries rather than as lists.

0.1.5
-----

- Simple case classes and case class hierarchies are now auto-serializable view Macros. No need to define your own implicit using `Case0ReadWriter` anymore!

0.1.4
-----

- Serialize numbers as JSON numbers instead of Strings.

0.1.3
-----

- Specification of the exception-throwing behavior: instead of failing with random `MatchError`s or similar, parse failures now are restricted to subclasses `upickle.Invalid` which define different failure modes.
