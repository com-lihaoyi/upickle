µPickle 0.1.7
=============

uPickle (pronounced micro-pickle) is a lightweight serialization library for Scala. It's key features are:

- [Less than 1000 lines of code](https://github.com/lihaoyi/upickle/graphs/contributors)
- [Zero-reflection 100% static serialization and deserialization](#supported-types)
- [Human-readable JSON encoding](#getting-started)
- [Zero dependencies](https://github.com/lihaoyi/upickle/blob/master/project/Build.scala)
- [Works in ScalaJS, allowing transfer of structured data between the JVM and Javascript](#scalajs)

Getting Started
===============

Add the following to your SBT config:

```scala
libraryDependencies += "com.lihaoyi" %% "upickle" % "0.1.7"
```

And then you can immediately start writing and reading common Scala objects to strings:

```scala
import upickle._
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
libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.1.7"
```

Other than that, everything is used the same way. upickle-0.1.7 is only compatible with ScalaJS 0.5.x.

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

Case classes in particular are serialized using the `apply` and `unapply` methods on their companion objects. This means that you can make your own classes serializable by giving them companions `apply` and `unapply`. `sealed` hierarchies are serialized as tagged unions: whatever the serialization of the actual object, together with an integer representing the position of the class in the hierarchy.   

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
write(12: Long)                  // res4: String = 12
write(12.3f: Float)              // res6: String = 12.3
write(12.3: Double)              // res7: String = 12.3
```

Even numbers too large for javascript are serialized as JSON numbers, since JSON doesn't specify an upper bound and uPickle can handle it just fine

```scala
write(4000000000000L: Long)      // res8: String = 4000000000000
```

Special values of `Double`s and `Float`s are serialized as Strings

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
write(Thing(1, "gg"))           // res23: String = {"a": 1, "b": "gg"}

case class Big(i: Int, b: Boolean, str: String, c: Char, d: Double)
write(Big(1, true, "lol", 'Z', 0.01)) 
// res24: String = {"i": 1, "b": true, "str": "lol", "c": "Z", "d": 0.01}
```

Sealed hierarchies are serialized as tagged values:

```scala
sealed trait IntOrTuple
case class IntThing(i: Int) extends IntOrTuple
case class TupleThing(name: String, t: (Int, Int)) extends IntOrTuple

write(IntThing(1))              // res25: String = [0, {"i": 1}]
write(TupleThing("naeem", (1, 2)))
// res26: String = [1, {"name": "naeem", "t": [1, 2]}]
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

Unsupported Things
==================

Here are some things which are not serializable using uPickle. One is mixed lists:
 
```scala
write(List(1, "omg"))
//<console>:11: error: could not find implicit value for evidence parameter of type upickle.Writer[List[Any]]
//              write(List(1, "omg"))
//                    ^
```

That is because serializing `Any` is not supported

```scala
write(1: Any)
<console>:11: error: could not find implicit value for evidence parameter of type upickle.Writer[Any]
              write(1: Any)
                   ^
```

If your case class has an `Any` member, it also can't be serialized:

```scala
case class AnyThing(x: Int, y: Any)
write(AnyThing(1, 2))
<console>:13: error: could not find implicit value for evidence parameter of type upickle.Writer[AnyThing]
              write(AnyThing(1, 2))
                   ^
```

You also can't serialize arbitrary classes; uPickle only supports case classes with a companion object with `apply`/`unapply` methods

```scala
class NewClass(i: Int)
write(new NewClass(1))
<console>:12: error: could not find implicit value for evidence parameter of type upickle.Writer[NewClass]
              write(new NewClass(1))
                   ^
```

Exceptions
==========

uPickle only throws exceptions on unpickling; if a pickler is properly defined, serializing a data structure to a `String` should never throw an exception.

On unpickling, uPickle throws one of two subclasses of `upickle.Invalid`:

- `upickle.Invalid.Json`: thrown when unpickling fails at the first step which attempst to convert the incoming `String` into semi-structured JSON data. The exception contains data about where parsing failed (`input`, `line`, `col`) as well as a human-readable error message.
- `upickle.Invalid.Data`: thrown when unpickling fails at the second step which attempts to convert the parsed JSON tree into structured data of the desired type. Contains the offending JSON subtree in `data`, along with a human-readable error message. 

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

uPickle is also a young project, and macro API in particular is filled with edge-cases, so there are probably other limitations not listed here

Why uPickle
===========

I wrote uPickle because I needed a transparent serialization library that worked both in Scala-JVM and Scala-JS, and my dissatisfaction with existing solutions:

- None of the libraries I could find were pure Scala: [spray-json](https://github.com/spray/spray-json) uses [parboiled1](https://github.com/sirthias/parboiled/wiki) which is written in Java, [play-json](http://www.playframework.com/documentation/2.2.x/ScalaJson) uses [Jackson](http://jackson.codehaus.org/), which uses Java/reflection. [scala-pickling](https://github.com/scala/pickling) silently falls back to reflection. This makes them difficult to port to ScalaJS, which supports neither Java nor reflection.
- Those libraries also typically have non-trivial dependency chains. That also makes them hard to port to ScalaJS, since I'd need to port all of their dependencies too.
- Lastly, many aim for a very ambitious target: untyped serialization of arbitrary object graphs. That forces them to use reflection, and makes their internals and semantics much more complex.

uPickle on the other hand aims much lower: by limiting the scope of the problem to statically-typed, tree-like, immutable data structures, it greatly simplifies both the internal implementation and the external API and behavior of the library. uPickle serializes objects using a very simple set of rules ("Does it have an implicit? Is it a class with `apply`/`unapply` on the companion?") that makes its behavior predictable and simple to understand.

Version History
===============

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
