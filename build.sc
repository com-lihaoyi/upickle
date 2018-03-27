import mill._, mill.scalalib._, mill.scalalib.publish._, mill.scalajslib._

trait CommonModule extends ScalaModule {

  def platformSegment: String

  def sources = T.sources(
    millSourcePath / "src",
    millSourcePath / s"src-$platformSegment"
  )
}
trait CommonPublishModule extends CommonModule with PublishModule with CrossScalaModule{
  def publishVersion = "0.6.2"
  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "com.lihaoyi",
    url = "https://github.com/lihaoyi/upickle",
    licenses = Seq(License.MIT),
    scm = SCM(
      "git://github.com/lihaoyi/upickle.git",
      "scm:git://github.com/lihaoyi/upickle.git"
    ),
    developers = Seq(
      Developer("lihaoyi", "Li Haoyi","https://github.com/lihaoyi")
    )
  )

  def docJar = T {
    import ammonite.ops._
    val outDir = T.ctx().dest

    val javadocDir = outDir / 'javadoc
    mkdir(javadocDir)

    val files = for{
      ref <- allSources()
      if exists(ref.path)
      p <- ls.rec(ref.path)
      if p.isFile
    } yield p.toNIO.toString

    val options = Seq("-d", javadocDir.toNIO.toString, "-usejavacp")

    if (files.nonEmpty) mill.modules.Jvm.subprocess(
      "scala.tools.nsc.ScalaDoc",
      scalaCompilerClasspath().map(_.path) ++ compileClasspath().filter(_.path.ext != "pom").map(_.path),
      mainArgs = (files ++ options).toSeq
    )

    mill.modules.Jvm.createJar(Agg(javadocDir))(outDir)
  }
}

trait JsonModule extends CommonPublishModule{
  def millSourcePath = build.millSourcePath / "ujson"
  trait JawnTestModule extends Tests with CommonModule{
    def platformSegment = JsonModule.this.platformSegment
    def ivyDeps = Agg(
      ivy"org.scalatest::scalatest::3.0.3",
      ivy"org.scalacheck::scalacheck::1.13.5"
    )
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }
}

object ujson extends Module{
  object js extends Cross[JsonJsModule]("2.11.11", "2.12.4")
  class JsonJsModule(val crossScalaVersion: String) extends JsonModule with ScalaJSModule {

    def scalaJSVersion = "0.6.22"
    def platformSegment = "js"

    object test extends JawnTestModule with TestScalaJSModule{
      def scalaJSVersion = "0.6.22"
    }
  }

  object jvm extends Cross[JsonJvmModule]("2.11.11", "2.12.4")
  class JsonJvmModule(val crossScalaVersion: String) extends JsonModule{
    def platformSegment = "jvm"

    object test extends JawnTestModule
  }

  object argonaut extends Cross[ArgonautModule]("2.11.11", "2.12.4")
  class ArgonautModule(val crossScalaVersion: String) extends CrossScalaModule{
    def moduleDeps = Seq(ujson.jvm())
    def ivyDeps = Agg(ivy"io.argonaut::argonaut:6.2")
  }
  object json4s extends Cross[Json4sModule]("2.11.11", "2.12.4")
  class Json4sModule(val crossScalaVersion: String) extends CrossScalaModule{
    def moduleDeps = Seq(ujson.jvm())
    def ivyDeps = Agg(
      ivy"org.json4s::json4s-ast:3.5.2",
      ivy"org.json4s::json4s-native:3.5.2"
    )
  }

  object circe extends Cross[CirceModule]("2.11.11", "2.12.4")
  class CirceModule(val crossScalaVersion: String) extends CrossScalaModule{
    def moduleDeps = Seq(ujson.jvm())
    def ivyDeps = Agg(ivy"io.circe::circe-parser:0.9.1")
  }

  object play extends Cross[PlayModule]("2.11.11", "2.12.4")
  class PlayModule(val crossScalaVersion: String) extends CrossScalaModule{
    def moduleDeps = Seq(ujson.jvm())
    def ivyDeps = Agg(
      ivy"com.typesafe.play::play-json:2.6.9",
      ivy"com.fasterxml.jackson.core:jackson-databind:2.9.4"
    )
  }
}

trait UpickleModule extends CommonPublishModule{
  def artifactName = "upickle"
  def millSourcePath = build.millSourcePath / "upickle"
  def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    ivy"com.lihaoyi::acyclic:0.1.5"
  )
  def compileIvyDeps = Agg(
    ivy"com.lihaoyi::acyclic:0.1.5",
    ivy"org.scala-lang:scala-reflect:${scalaVersion()}",
    ivy"org.scala-lang:scala-compiler:${scalaVersion()}"
  )
  def scalacOptions = Seq(
    "-unchecked",
    "-deprecation",
    "-encoding", "utf8",
    "-feature"
  )


  def generatedSources = T{
    val dir = T.ctx().dest
    val file = dir / "upickle" / "Generated.scala"
    ammonite.ops.mkdir(dir / "upickle")
    val tuples = (1 to 22).map{ i =>
      def commaSeparated(s: Int => String) = (1 to i).map(s).mkString(", ")
      val writerTypes = commaSeparated(j => s"T$j: Writer")
      val readerTypes = commaSeparated(j => s"T$j: Reader")
      val typeTuple = commaSeparated(j => s"T$j")
      val implicitWriterTuple = commaSeparated(j => s"implicitly[Writer[T$j]]")
      val implicitReaderTuple = commaSeparated(j => s"implicitly[Reader[T$j]]")
      val lookupTuple = commaSeparated(j => s"x(${j-1})")
      val fieldTuple = commaSeparated(j => s"x._$j")
      val caseReader =
        if(i == 1) s"f(readJs[Tuple1[T1]](x)._1)"
        else s"f.tupled(readJs[Tuple$i[$typeTuple]](x))"
        s"""
        implicit def Tuple${i}Writer[$writerTypes]: TupleNWriter[Tuple$i[$typeTuple]] =
          new TupleNWriter[Tuple$i[$typeTuple]](Array($implicitWriterTuple), x => if (x == null) null else Array($fieldTuple))
        implicit def Tuple${i}Reader[$readerTypes]: TupleNReader[Tuple$i[$typeTuple]] =
          new TupleNReader(Array($implicitReaderTuple), x => Tuple$i($lookupTuple).asInstanceOf[Tuple$i[$typeTuple]])
        """
    }

    ammonite.ops.write(file, s"""
      package upickle
      package api
      import acyclic.file
      import language.experimental.macros
      /**
       * Auto-generated picklers and unpicklers, used for creating the 22
       * versions of tuple-picklers and case-class picklers
       */
      trait Generated extends upickle.core.Types{
        ${tuples.mkString("\n")}
      }
    """)
    Seq(PathRef(dir))
  }
  trait UpickleTestModule extends Tests with CommonModule{
    def platformSegment = UpickleModule.this.platformSegment
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest::0.6.4",
      ivy"com.lihaoyi::acyclic:0.1.5"
    )

    def testFrameworks = Seq("upickle.UTestFramework")
  }
}



object upickle extends Module{
  object jvm extends Cross[UpickleJvmModule]("2.11.11", "2.12.4")
  class UpickleJvmModule(val crossScalaVersion: String) extends UpickleModule{
    def platformSegment = "jvm"
    def moduleDeps = Seq(ujson.jvm())

    object test extends UpickleTestModule{
      def moduleDeps = super.moduleDeps ++ Seq(
        ujson.argonaut(),
        ujson.circe(),
        ujson.json4s(),
        ujson.play(),
      )
      def ivyDeps = super.ivyDeps() ++ bench.jvm.ivyDeps()
    }
  }

  object js extends Cross[UpickleJsModule]("2.11.11", "2.12.4")
  class UpickleJsModule(val crossScalaVersion: String) extends UpickleModule with ScalaJSModule {
    def moduleDeps = Seq(ujson.js())
    def platformSegment = "js"

    def scalaJSVersion = "0.6.22"
    def scalacOptions = T{
      super.scalacOptions() ++ Seq({
        val a = build.millSourcePath.toString.replaceFirst("[^/]+/?$", "")
        val g = "https://raw.githubusercontent.com/lihaoyi/upickle"
        s"-P:scalajs:mapSourceURI:$a->$g/v${publishVersion()}/"
      })
    }
    object test extends UpickleTestModule with TestScalaJSModule{
      def scalaJSVersion = "0.6.22"
    }
  }
}

trait BenchModule extends CommonModule{
  def scalaVersion = "2.12.4"
  def millSourcePath = build.millSourcePath / "bench"
  def ivyDeps = Agg(
    ivy"io.circe::circe-core::0.9.1",
    ivy"io.circe::circe-generic::0.9.1",
    ivy"io.circe::circe-parser::0.9.1",
    ivy"com.typesafe.play::play-json::2.6.7",
    ivy"io.argonaut::argonaut:6.2",
    ivy"org.json4s::json4s-ast:3.5.2",
    ivy"com.lihaoyi::sourcecode:0.1.4",
    ivy"com.avsystem.commons::commons-core:1.26.2",
  )
}

object bench extends Module {
  object js extends BenchModule with ScalaJSModule {
    def scalaJSVersion = "0.6.22"
    def platformSegment = "js"
    def moduleDeps = Seq(upickle.js("2.12.4").test)
    def run(args: String*) = T.command {
      finalMainClassOpt() match{
        case Left(err) => mill.eval.Result.Failure(err)
        case Right(_) =>
          ScalaJSBridge.scalaJSBridge().run(
            toolsClasspath().map(_.path),
            nodeJSConfig(),
            fullOpt().path.toIO
          )
          mill.eval.Result.Success(())
      }

    }
  }

  object jvm extends BenchModule {
    def platformSegment = "jvm"
    def moduleDeps = Seq(upickle.jvm("2.12.4").test)
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"com.fasterxml.jackson.module::jackson-module-scala:2.9.4",
      ivy"com.fasterxml.jackson.core:jackson-databind:2.9.4",
    )
  }
}
