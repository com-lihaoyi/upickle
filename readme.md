µPickle 0.1.5
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
libraryDependencies += "com.lihaoyi" %% "upickle" % "0.1.5"
```

And then you can immediately start writing and reading common Scala objects to strings:

```scala
scala> import upickle._
import upickle._

scala> write(1)
res1: String = 1

scala> write(Seq(1, 2, 3))
res2: String = [1, 2, 3]

scala> read[Seq[Int]]("[1, 2, 3]")
res3: Seq[Int] = List(1, 2, 3)

scala> write((1, "omg", true))
res4: String = [1, "omg", true]

scala> read[(Int, String, Boolean)]("""[1, "omg", true]""")
res5: (Int, String, Boolean) = (1,omg,true)
```

ScalaJS
=======

For ScalaJS applications, use this dependencies instead:

```scala
libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.1.5"
```

Other than that, everything is used the same way. upickle-0.1.5 is only compatible with ScalaJS 0.5.x.

Supported Types
===============

Out of the box, uPickle supports writing and reading the following types:

- `Boolean`, `Byte`, `Char`, `Short`, `Int`, `Long`, `Float`, `Double`
- `Tuple`s from 1 to 6
- `Seq`, `List`, `Vector`, `Set`, `SortedSet`, `Option`, `Array`, `Map`s
- `Duration`, `Either`
- Stand-alone `case class`es and `case object`s
- `case class`es and `case object`s that are part of a `sealed trait` or `sealed class` hierarchy
- `sealed trait` and `sealed class`es themselves, assuming that all subclasses are picklable

Readability/writability is recursive: a container such as a `Tuple` or `case class` is only readable if all its contents are readable, and only writable if all its contents are writable. That means that you cannot serialize a `List[Any]`, since uPickle doesn't provide a generic way of serializing `Any`.

Case classes in particular are serialized using the `apply` and `unapply` methods on their companion objects. This means that you can make your own classes serializable by giving them companions `apply` and `unapply`. `sealed` hierarchies are serialized as tagged unions: whatever the serialization of the actual object, together with an integer representing the   

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
- Read/writing arbitrarily shaped objects

Most of these limitations are inherent in the fact that ScalaJS does not support reflection, and are unlikely to ever go away.

uPickle is also a young project, and macro API in particular is filled with edge-cases

Why uPickle
===========

I wrote uPickle because I needed a transparent serialization library that worked both in Scala-JVM and Scala-JS, and my dissatisfaction with existing solutions:

- None of the libraries I could find were pure Scala: [spray-json](https://github.com/spray/spray-json) uses [parboiled1](https://github.com/sirthias/parboiled/wiki) which is written in Java, [play-json](http://www.playframework.com/documentation/2.2.x/ScalaJson) uses [Jackson]http://jackson.codehaus.org/(), which uses Java/reflection. [scala-pickling](https://github.com/scala/pickling) silently falls back to reflection. This makes them difficult to port to ScalaJS, which supports neither Java nor reflection.
- Those libraries also typically have non-trivial dependency chains. That also makes them hard to port to ScalaJS, since I'd need to port all of their dependencies too.
- Lastly, many aim for a very ambitious target: untyped serialization of arbitrary object graphs. That forces them to use reflection, and makes their internals and semantics much more complex.

uPickle on the other hand aims much lower:

- Only pickling statically-known-types and sealed class hierarchies
- No support at all for pickling untyped values
- Only pickling "struct"-style case classes by default, and not arbitrary classes
- No dependencies
