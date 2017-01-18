
val settings = Seq(
  organization := "com.lihaoyi",
  version := upicklePPrint.Constants.version,

  scalaVersion := "2.12.0",
  crossScalaVersions := Seq("2.10.6", "2.11.8", "2.12.0"),

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
    "com.lihaoyi" %%% "utest" % "0.4.4" % "test",
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
      </developers>
)

settings

val compatible211andAbove =
  unmanagedSourceDirectories in Compile ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n >= 11 =>
        Seq(baseDirectory.value / ".." / "shared" / "src" / "main" / "scala-2.11+")
      case _ =>
        Seq()
    }
  }

val derive = crossProject.settings(settings).settings(
  name := "derive",
  compatible211andAbove
)
val deriveJS = derive.js
val deriveJVM = derive.jvm
val upickle = crossProject
  .dependsOn(derive % "compile->compile;test->test")
  .settings(settings:_*)
  .settings(
    name := "upickle",
    compatible211andAbove,
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
    libraryDependencies += "org.spire-math" %% "jawn-parser" % "0.10.3"
  )

lazy val upickleJS = upickle.js
lazy val upickleJVM = upickle.jvm
lazy val test = project
  .in(file("test"))
  .dependsOn(upickleJVM, pprintJVM, deriveJVM % "compile->compile;test->test;test->compile;compile->test")
  .settings(settings)

lazy val pprint = crossProject
  .dependsOn(derive % "compile->compile;test->test")
  .settings(settings:_*)
  .settings(
    compatible211andAbove,
    name := "pprint",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "fansi" % "0.2.3",
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
      "com.chuusai" %%% "shapeless" % "2.3.2" % "test"
    ),
    sourceGenerators in Compile += Def.task {
      val dir = (sourceManaged in Compile).value
      val file = dir/"pprint"/"PPrintGen.scala"
      val tuples = (1 to 22).map{ i =>
        val ts = (1 to i) map ("T" + _)
        val chunks = 1 to i map { n =>
          s"render(t._$n, cfg)"
        }
        val commaTs = ts.mkString(", ")
        val tupleType = s"Tuple$i[$commaTs]"
        val boundedTypes = ts.map(_ + ": PP").mkString(",")
        s"""
        implicit def Tuple${i}Chunker[$boundedTypes]: Chunker[$tupleType] = makeChunker{
          (t: $tupleType, cfg: C) => Iterator(${chunks.mkString(",")})
        }
        """
      }
      val output = s"""
        package pprint
        trait PPrinterGen extends GenUtils{
          ${tuples.mkString("\n")}
        }

      """.stripMargin
      IO.write(file, output)
      Seq(file)
    }.taskValue,
    sourceGenerators in Compile += Def.task {
      val dir = (sourceManaged in Compile).value
      val file = dir/"pprint"/"TPrintGen.scala"

      val typeGen = for(i <- 2 to 22) yield {
        val ts = (1 to i).map("T" + _).mkString(", ")
        val tsBounded = (1 to i).map("T" + _ + ": Type").mkString(", ")
        val tsGet = (1 to i).map("get[T" + _ + "](cfg)").mkString(" + \", \" + ")
        s"""
          implicit def F${i}TPrint[$tsBounded, R: Type] = make[($ts) => R](cfg =>
            "(" + $tsGet + ") => " + get[R](cfg)
          )
          implicit def T${i}TPrint[$tsBounded] = make[($ts)](cfg =>
            "(" + $tsGet + ")"
          )
        """
      }
      val output = s"""
        package pprint
        trait TPrintGen[Type[_], Cfg]{
          def make[T](f: Cfg => String): Type[T]
          def get[T: Type](cfg: Cfg): String
          implicit def F0TPrint[R: Type] = make[() => R](cfg => "() => " + get[R](cfg))
          implicit def F1TPrint[T1: Type, R: Type] = {
            make[T1 => R](cfg => get[T1](cfg) + " => " + get[R](cfg))
          }
          ${typeGen.mkString("\n")}
        }
      """.stripMargin
      IO.write(file, output)
      Seq(file)
    }.taskValue
  )
  .jvmSettings( 
    libraryDependencies ++= {
      if (Set("2.10", "2.11").contains(scalaBinaryVersion.value))
        Seq(
          "org.spire-math" %% "spire" % "0.11.0" % "test",
          "com.typesafe.akka" %% "akka-http-experimental" % "1.0-M3" % "test",
          "com.twitter" %% "finagle-httpx" % "6.26.0" % "test",
          "org.tpolecat" %% "doobie-core" % "0.2.3" % "test"
        )
      else Seq() // not yet available for 2.12?
    },
    unmanagedSourceDirectories in Compile ++= {
      if (Set("2.10", "2.11").contains(scalaBinaryVersion.value)) 
        Seq(baseDirectory.value / ".." / "shared" / "src" / "main" / "scala-2.10_2.11")
      else
        Seq() // skip 2.12
    }
  )

lazy val pprintJVM = pprint.jvm
lazy val pprintJS = pprint.js
lazy val modules = project.settings(settings).aggregate(pprintJVM, pprintJS, upickleJVM, upickleJS, deriveJS, deriveJVM).settings(
  publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo"))),
  publishArtifact := false
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

lazy val pprintReadme = scalatex.ScalatexReadme(
  projectId = "pprintReadme",
  wd = file(""),
  url = "https://github.com/lihaoyi/upickle/tree/master",
  source = "Readme"
).settings(
  scalaVersion := "2.11.8",
  (unmanagedSources in Compile) += baseDirectory.value/".."/"project"/"Constants.scala"
)
