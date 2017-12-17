

crossScalaVersions := Seq("2.11.11", "2.12.4")

val upickle = crossProject

  .settings(
    name := "upickle",
    organization := "com.lihaoyi",
    version := _root_.upickle.Constants.version,

    scalaVersion := "2.12.4",
    crossScalaVersions := Seq("2.11.11", "2.12.4"),

    scalacOptions := Seq("-unchecked",
      "-deprecation",
      "-encoding", "utf8",
      "-feature"),
    // Sonatype
    publishArtifact in Test := false,
    publishTo := Some("releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"),

    testFrameworks += new TestFramework("utest.runner.Framework"),
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "acyclic" % "0.1.5" % "provided",
      "com.lihaoyi" %%% "utest" % "0.5.4" % "test",
      "com.lihaoyi" %%% "sourcecode" % "0.1.3",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
    ) ++ (
      if(scalaBinaryVersion.value == "2.10")
        Seq(
          compilerPlugin("org.scalamacros" % s"paradise" % "2.1.0" cross CrossVersion.full),
          "org.scalamacros" %% s"quasiquotes" % "2.1.0"
        )
      else Seq()
      ),
    scalaJSStage in Global := FullOptStage,
    autoCompilerPlugins := true,
    //  scalacOptions += "-Xlog-implicits",
    addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.5"),
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
        </developers>,

    sourceGenerators in Compile += Def.task{
      val dir = (sourceManaged in Compile).value

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
          implicit def Tuple${i}W[$writerTypes] = makeWriter[Tuple${i}[$typeTuple]](
            x => Js.Arr($written)
          )
          implicit def Tuple${i}R[$readerTypes] = makeReader[Tuple${i}[$typeTuple]](
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
          trait Generated extends GeneratedUtil{
            ${tuples.mkString("\n")}
          }
        """)
      Seq(file)
    }.taskValue
  ).jsSettings(
    scalaJSStage in Test := FullOptStage,
      scalacOptions ++= (if (isSnapshot.value) Seq.empty else Seq({
        val a = baseDirectory.value.toURI.toString.replaceFirst("[^/]+/?$", "")
        val g = "https://raw.githubusercontent.com/lihaoyi/upickle"
        s"-P:scalajs:mapSourceURI:$a->$g/v${version.value}/"
      }))
  ).jvmSettings(
    libraryDependencies += "org.spire-math" %% "jawn-parser" % "0.11.0"
  )

lazy val upickleJS = upickle.js
lazy val upickleJVM = upickle.jvm
lazy val test = project
  .in(file("test"))
  .dependsOn(upickleJVM)
  .settings(
    scalaVersion := "2.12.4"
  )


lazy val upickleReadme = scalatex.ScalatexReadme(
  projectId = "upickleReadme",
  wd = file(""),
  url = "https://github.com/lihaoyi/upickle/tree/master",
  source = "Readme"
).settings(
  scalaVersion := "2.11.8",
  (unmanagedSources in Compile) += baseDirectory.value/".."/"project"/"Constants.scala"
)
