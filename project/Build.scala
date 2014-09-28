import sbt._
import Keys._

import scala.scalajs.sbtplugin.env.nodejs.NodeJSEnv
import scala.scalajs.sbtplugin.ScalaJSPlugin._

import scala.scalajs.sbtplugin.ScalaJSPlugin.ScalaJSKeys._

object Build extends sbt.Build{
  val cross = new utest.jsrunner.JsCrossBuild(
    organization := "com.lihaoyi",

    version := "0.2.5",
    scalaVersion := "2.10.4",
    name := "upickle",

    // Sonatype
    publishArtifact in Test := false,
    publishTo <<= version { (v: String) =>
      Some("releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
    },
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "acyclic" % "0.1.2" % "provided",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
    ) ++ (
      if (scalaVersion.value startsWith "2.11.") Nil
      else Seq( 
        "org.scalamacros" %% s"quasiquotes" % "2.0.0" % "provided",
        compilerPlugin("org.scalamacros" % s"paradise" % "2.0.0" cross CrossVersion.full)
      )
    ),

    sourceGenerators in Compile <+= sourceManaged in Compile map { dir =>
      val file = dir / "upickle" / "Generated.scala"
      val tuplesAndCases = (1 to 22).map{ i =>
        def commaSeparated(s: Int => String) = (1 to i).map(s).mkString(", ")
        val writerTypes = commaSeparated(j => s"T$j: Writer")
        val readerTypes = commaSeparated(j => s"T$j: Reader")
        val typeTuple = commaSeparated(j => s"T$j")
        val written = commaSeparated(j => s"writeJs(x._$j)")
        val pattern = commaSeparated(j => s"x$j")
        val read = commaSeparated(j => s"readJs[T$j](x$j)")
        val caseReader =
          if(i == 1) s"f(readJs[Tuple1[T1]](x)._1)"
          else s"f.tupled(readJs[Tuple$i[$typeTuple]](x))"

        (s"""
        implicit def Tuple${i}W[$writerTypes] = W[Tuple${i}[$typeTuple]](
          x => Js.Arr($written)
        )
        implicit def Tuple${i}R[$readerTypes] = R[Tuple${i}[$typeTuple]](
          validate("Array(${i})"){case Js.Arr($pattern) => Tuple${i}($read)}
        )
        """, s"""
        def Case${i}R[$readerTypes, V]
                     (f: ($typeTuple) => V, names: Array[String], defaults: Array[Js.Value])
          = RCase[V](names, defaults, {case x => $caseReader})

        def Case${i}W[$writerTypes, V]
                     (g: V => Option[Tuple${i}[$typeTuple]], names: Array[String], defaults: Array[Js.Value])
          = WCase[V](names, defaults, x => writeJs(g(x).get))
        """)
      }

      val (tuples, cases) = tuplesAndCases.unzip

      IO.write(file, s"""
        package upickle
        import acyclic.file
        import language.experimental.macros
        /**
         * Auto-generated picklers and unpicklers, used for creating the 22
         * versions of tuple-picklers and case-class picklers
         */
        trait Generated extends Types with GeneratedUtil{
          import Aliases._

          ${tuples.mkString("\n")}
          /**
           * Contains the 22 case-class picklers, since although they are not
           * part of the public API, you probably shouldn't be handling them
           * directly.
           */
           trait InternalGenerated{
            ${cases.mkString("\n")}
          }
        }
      """)
      Seq(file)
    },
    autoCompilerPlugins := true,

    addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.2"),
    pomExtra :=
      <url>https://github.com/lihaoyi/upickle</url>
      <licenses>
        <license>
          <name>MIT license</name>
          <url>http://www.opensource.org/licenses/mit-license.php</url>
        </license>
      </licenses>
      <scm>
        <url>git://github.com/lihaoyi/upickle.git</url>
        <connection>scm:git://github.com/lihaoyi/upickle.git</connection>
      </scm>
      <developers>
        <developer>
          <id>lihaoyi</id>
          <name>Li Haoyi</name>
          <url>https://github.com/lihaoyi</url>
        </developer>
      </developers>

  )

  lazy val root = cross.root

  lazy val js = cross.js.settings(
    (jsEnv in Test) := new NodeJSEnv
  )

  lazy val jvm = cross.jvm.settings(
    libraryDependencies += "org.spire-math" %% "jawn-parser" % "0.6.0"
  )
}

