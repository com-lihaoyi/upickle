// plugins
import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version::0.3.0`
import $ivy.`com.github.lolgab::mill-mima::0.0.13`

// imports
import mill._
import mill.define.Target
import mill.scalalib._
import mill.scalalib.publish._
import mill.scalajslib._
import mill.scalanativelib._
import mill.modules._
import mill.scalalib.api.Util.isScala3
import mill.scalanativelib.api.{LTO, ReleaseMode}
import de.tobiasroeser.mill.vcs.version.VcsVersion
import com.github.lolgab.mill.mima._

val scala212  = "2.12.17"
val scala213  = "2.13.10"

val scala3   = "3.2.2"
val scalaJS  = "1.13.0"
val scalaNative = "0.4.10"
val acyclic = "0.3.6"
val sourcecode = "0.3.0"

val dottyCustomVersion = Option(sys.props("dottyVersion"))

val scala2JVMVersions = Seq(scala212, scala213)
val scalaJVMVersions = scala2JVMVersions ++ Seq(scala3) ++ dottyCustomVersion

val scalaJSVersions = scalaJVMVersions.map((_, scalaJS))
val scalaNativeVersions = scalaJVMVersions.map((_, scalaNative))

trait CommonBaseModule extends ScalaModule {
  def platformSegment: String

  def sources = T.sources{
    super.sources() ++
    Seq(PathRef(millSourcePath / s"src-$platformSegment")) ++
    (if (scalaVersion() != scala212) {
      Seq(PathRef(millSourcePath / "src-2.13+"))
    } else Seq()) ++
    (platformSegment match {
      case "jvm" => Seq(PathRef(millSourcePath / "src-jvm-native"))
      case "native" => Seq(PathRef(millSourcePath / "src-js-native"), PathRef(millSourcePath / "src-jvm-native"))
      case "js" => Seq(PathRef(millSourcePath / "src-js-native"))
    })
  }
}

trait CommonModule extends CommonBaseModule {
  def scalacOptions = T{
    super.scalacOptions() ++ {
      if (scalaVersion() == scala212) Seq("-opt:l:method") else Nil
    } ++ Seq(
      "-unchecked",
      "-deprecation",
      "-encoding", "utf8",
      "-feature",
      "-Xfatal-warnings"
    )
  }
}

trait CommonPublishModule extends CommonModule with PublishModule with Mima with CrossScalaModule {

  def publishVersion = VcsVersion.vcsState().format()
  def mimaPreviousVersions = Seq("3.0.0")
  def isDotty = crossScalaVersion.startsWith("0") || crossScalaVersion.startsWith("3")
  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "com.lihaoyi",
    url = "https://github.com/lihaoyi/upickle",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github(owner = "com-lihaoyi", repo = "upickle"),
    developers = Seq(
      Developer("lihaoyi", "Li Haoyi","https://github.com/lihaoyi")
    )
  )

  def publishProperties: Target[Map[String, String]] = super.publishProperties() ++ Map(
    "info.releaseNotesURL" -> "https://com-lihaoyi.github.io/upickle/#VersionHistory"
  )
  def versionScheme: T[Option[VersionScheme]] = T(Some(VersionScheme.SemVerSpec))
  def templates = T.sources(millSourcePath / "templates", millSourcePath / s"templates-$platformSegment")
  def generatedSources = T{
    for{
      pathRef <- templates()
      p <- if (os.exists(pathRef.path)) os.list(pathRef.path) else Nil
      rename <- Seq("Char", "Byte")
    }{
      os.write(
        T.dest / p.last.replace("Elem", rename).replace("elem", rename.toLowerCase),
        os.read(p).replace("Elem", rename).replace("elem", rename.toLowerCase)
      )
    }
    Seq(PathRef(T.dest))
  }
  def docJar = {
    if (isScala3(crossScalaVersion)) T {
      val outDir = T.ctx().dest
      val javadocDir = outDir / "javadoc"
      os.makeDir.all(javadocDir)
      mill.modules.Jvm.createJar(Agg(javadocDir))(outDir)
    } else {
      super.docJar
    }
  }
}

trait CommonTestModule extends CommonBaseModule with TestModule.Utest{
  def ivyDeps = Agg(ivy"com.lihaoyi::utest::0.8.1") ++ (
    if (isScala3(scalaVersion())) Agg.empty[mill.scalalib.Dep]
    else Agg(ivy"com.lihaoyi:::acyclic:$acyclic")
  )
  def docJar = T {
    val outDir = T.ctx().dest
    val javadocDir = outDir / "javadoc"
    os.makeDir.all(javadocDir)
    mill.modules.Jvm.createJar(Agg(javadocDir))(outDir)
  }
  def scalacOptions = super.scalacOptions() ++
    (if (isScala3(scalaVersion())) Seq(
      "-Ximplicit-search-limit",
      "200000"
    ) else Seq.empty[String])
}

trait CommonJvmModule extends CommonPublishModule{
  def platformSegment = "jvm"
  def millSourcePath = super.millSourcePath / os.up
  trait Tests extends super.Tests with CommonTestModule{
    def platformSegment = "jvm"
  }
}
trait CommonJsModule extends CommonPublishModule with ScalaJSModule{
  def platformSegment = "js"
  def crossScalaJSVersion: String
  def scalaJSVersion = crossScalaJSVersion
  
  private def sourceMapOptions = T.task {
    val vcsState = VcsVersion.vcsState()
    vcsState.lastTag.collect {
      case tag if vcsState.commitsSinceLastTag == 0 =>
        val baseUrl = pomSettings().url.replace("github.com", "raw.githubusercontent.com")
        val sourcesOptionName = if(isScala3(crossScalaVersion)) "-scalajs-mapSourceURI" else "-P:scalajs:mapSourceURI"
        s"$sourcesOptionName:${T.workspace.toIO.toURI}->$baseUrl/$tag/"
    }
  }
  def scalacOptions = super.scalacOptions() ++ sourceMapOptions()

  def millSourcePath = super.millSourcePath / os.up / os.up
  trait Tests extends super.Tests with CommonTestModule{
    def platformSegment = "js"
    def scalaJSVersion = crossScalaJSVersion
  }
}

trait CommonNativeModule extends CommonPublishModule with ScalaNativeModule{
  def platformSegment = "native"
  def crossScalaNativeVersion: String
  def scalaNativeVersion = crossScalaNativeVersion
  def millSourcePath = super.millSourcePath / os.up / os.up
  trait Tests extends super.Tests with CommonTestModule{
    def platformSegment = "native"
    def scalaNativeVersion = crossScalaNativeVersion
  }
}

trait CommonCoreModule extends CommonPublishModule {
  def artifactName = "upickle-core"
  def ivyDeps = Agg(ivy"com.lihaoyi::geny::1.0.0")
}
object core extends Module {
  object js extends Cross[CoreJsModule](scalaJSVersions:_*)

  class CoreJsModule(val crossScalaVersion: String, val crossScalaJSVersion: String) extends CommonJsModule with CommonCoreModule{
    object test extends Tests
  }

  object jvm extends Cross[CoreJvmModule](scalaJVMVersions:_*)
  class CoreJvmModule(val crossScalaVersion: String) extends CommonJvmModule with CommonCoreModule{
    object test extends Tests
  }

  object native extends Cross[CoreNativeModule](scalaNativeVersions:_*)
  class CoreNativeModule(val crossScalaVersion: String, val crossScalaNativeVersion: String) extends CommonNativeModule with CommonCoreModule{
    object test extends Tests
  }
}

object implicits extends Module {

  trait ImplicitsModule extends CommonPublishModule{
    def compileIvyDeps = if (!isDotty) Agg(
      ivy"com.lihaoyi:::acyclic:$acyclic",
      ivy"org.scala-lang:scala-reflect:${scalaVersion()}"
    )
    else Agg.empty[Dep]
    def generatedSources = T{
      val dir = T.ctx().dest
      val file = dir / "upickle" / "Generated.scala"
      os.makeDir(dir / "upickle")
      val tuples = (1 to 22).map{ i =>
        def commaSeparated(s: Int => String) = (1 to i).map(s).mkString(", ")
        val writerTypes = commaSeparated(j => s"T$j: Writer")
        val readerTypes = commaSeparated(j => s"T$j: Reader")
        val typeTuple = commaSeparated(j => s"T$j")
        val implicitWriterTuple = commaSeparated(j => s"implicitly[Writer[T$j]]")
        val implicitReaderTuple = commaSeparated(j => s"implicitly[Reader[T$j]]")
        val lookupTuple = commaSeparated(j => s"x(${j-1})")
        val fieldTuple = commaSeparated(j => s"x._$j")
        s"""
        implicit def Tuple${i}Writer[$writerTypes]: TupleNWriter[Tuple$i[$typeTuple]] =
          new TupleNWriter[Tuple$i[$typeTuple]](Array($implicitWriterTuple), x => if (x == null) null else Array($fieldTuple))
        implicit def Tuple${i}Reader[$readerTypes]: TupleNReader[Tuple$i[$typeTuple]] =
          new TupleNReader(Array($implicitReaderTuple), x => Tuple$i($lookupTuple).asInstanceOf[Tuple$i[$typeTuple]])
        """
      }

      os.write(file, s"""
      package upickle.implicits
      /**
       * Auto-generated picklers and unpicklers, used for creating the 22
       * versions of tuple-picklers and case-class picklers
       */
      trait Generated extends TupleReadWriters{
        ${tuples.mkString("\n")}
      }
    """)
      Seq(PathRef(dir))
    }

  }
  object js extends Cross[JsModule](scalaJSVersions:_*)

  class JsModule(val crossScalaVersion: String, val crossScalaJSVersion: String) extends ImplicitsModule with CommonJsModule{
    def moduleDeps = Seq(core.js(crossScalaVersion, crossScalaJSVersion))
    def artifactName = "upickle-implicits"

    object test extends Tests {
      def moduleDeps = super.moduleDeps ++ Seq(
        ujson.js(crossScalaVersion, crossScalaJSVersion).test,
        core.js(crossScalaVersion, crossScalaJSVersion).test
      )
    }
  }

  object jvm extends Cross[JvmModule](scalaJVMVersions:_*)
  class JvmModule(val crossScalaVersion: String) extends ImplicitsModule with CommonJvmModule{
    def moduleDeps = Seq(core.jvm())
    def artifactName = "upickle-implicits"
    object test extends Tests {
      def moduleDeps = super.moduleDeps ++ Seq(ujson.jvm().test, core.jvm().test)
    }
  }

  object native extends Cross[NativeModule](scalaNativeVersions:_*)

  class NativeModule(val crossScalaVersion: String, val crossScalaNativeVersion: String) extends ImplicitsModule with CommonNativeModule{
    def moduleDeps = Seq(core.native(crossScalaVersion, crossScalaNativeVersion))
    def artifactName = "upickle-implicits"

    object test extends Tests {
      def moduleDeps = super.moduleDeps ++ Seq(core.native(crossScalaVersion, crossScalaNativeVersion).test)
    }
  }
}

object upack extends Module {

  object js extends Cross[JsModule](scalaJSVersions:_*)

  class JsModule(val crossScalaVersion: String, val crossScalaJSVersion: String) extends CommonJsModule {
    def moduleDeps = Seq(core.js(crossScalaVersion, crossScalaJSVersion))
    def artifactName = "upack"

    object test extends Tests {
      def moduleDeps = super.moduleDeps ++ Seq(
        ujson.js(crossScalaVersion, crossScalaJSVersion).test,
        core.js(crossScalaVersion, crossScalaJSVersion).test
      )
    }
  }

  object jvm extends Cross[JvmModule](scalaJVMVersions:_*)
  class JvmModule(val crossScalaVersion: String) extends CommonJvmModule {
    def moduleDeps = Seq(core.jvm())
    def artifactName = "upack"
    object test extends Tests {
      def moduleDeps = super.moduleDeps ++ Seq(ujson.jvm().test, core.jvm().test)
    }
  }

  object native extends Cross[NativeModule](scalaNativeVersions:_*)

  class NativeModule(val crossScalaVersion: String, val crossScalaNativeVersion: String) extends CommonNativeModule {
    def moduleDeps = Seq(core.native(crossScalaVersion, crossScalaNativeVersion))
    def artifactName = "upack"

    object test extends Tests {
      def moduleDeps = super.moduleDeps ++ Seq(
        ujson.native(crossScalaVersion, crossScalaNativeVersion).test,
        core.native(crossScalaVersion, crossScalaNativeVersion).test
      )
    }
  }
}


object ujson extends Module{
  trait JsonModule extends CommonPublishModule{
    def artifactName = "ujson"
  }

  object js extends Cross[JsModule](scalaJSVersions:_*)
  class JsModule(val crossScalaVersion: String, val crossScalaJSVersion: String) extends JsonModule with CommonJsModule{
    def moduleDeps = Seq(core.js(crossScalaVersion, crossScalaJSVersion))

    object test extends Tests with CommonTestModule{
      def moduleDeps = super.moduleDeps ++ Seq(core.js(crossScalaVersion, crossScalaJSVersion).test)
    }
  }

  object jvm extends Cross[JvmModule](scalaJVMVersions:_*)
  class JvmModule(val crossScalaVersion: String) extends JsonModule with CommonJvmModule{
    def moduleDeps = Seq(core.jvm())
    object test extends Tests with CommonTestModule{
      def moduleDeps = super.moduleDeps ++ Seq(core.jvm().test)
    }
  }

  object native extends Cross[NativeModule](scalaNativeVersions:_*)
  class NativeModule(val crossScalaVersion: String, val crossScalaNativeVersion: String) extends JsonModule with CommonNativeModule{
    def moduleDeps = Seq(core.native(crossScalaVersion, crossScalaNativeVersion))

    object test extends Tests with CommonTestModule{
      def moduleDeps = super.moduleDeps ++ Seq(core.native(crossScalaVersion, crossScalaNativeVersion).test)
    }
  }

  object argonaut extends Cross[ArgonautModule](scala2JVMVersions:_*)
  class ArgonautModule(val crossScalaVersion: String) extends CommonPublishModule{
    def artifactName = "ujson-argonaut"
    def platformSegment = "jvm"
    def moduleDeps = Seq(ujson.jvm())
    def ivyDeps = Agg(ivy"io.argonaut::argonaut:6.2.6")
  }
  object json4s extends Cross[Json4sModule](scalaJVMVersions:_*)
  class Json4sModule(val crossScalaVersion: String) extends CommonPublishModule{
    def artifactName = "ujson-json4s"
    def platformSegment = "jvm"
    def moduleDeps = Seq(ujson.jvm())
    def ivyDeps = Agg(
      ivy"org.json4s::json4s-ast:4.0.6",
      ivy"org.json4s::json4s-native:4.0.6"
    )
  }

  object circe extends Cross[CirceModule](scalaJVMVersions:_*)
  class CirceModule(val crossScalaVersion: String) extends CommonPublishModule{
    def artifactName = "ujson-circe"
    def platformSegment = "jvm"
    def moduleDeps = Seq(ujson.jvm())
    val circeVersion = "0.14.5"
    def ivyDeps = Agg(ivy"io.circe::circe-parser:$circeVersion")
  }

  object play extends Cross[PlayModule](scala2JVMVersions:_*)
  class PlayModule(val crossScalaVersion: String) extends CommonPublishModule{
    def artifactName = "ujson-play"
    def platformSegment = "jvm"
    def moduleDeps = Seq(ujson.jvm())
    val playJsonVersion = "2.9.4"
    def ivyDeps = Agg(
      ivy"com.typesafe.play::play-json:$playJsonVersion"
    )
  }
}

trait UpickleModule extends CommonPublishModule{
  def artifactName = "upickle"
  def compileIvyDeps = if (!isDotty) Agg(
    ivy"com.lihaoyi:::acyclic:$acyclic",
    ivy"org.scala-lang:scala-reflect:${scalaVersion()}",
    ivy"org.scala-lang:scala-compiler:${scalaVersion()}"
  )
  else Agg.empty[Dep]
}


object upickle extends Module{
  object jvm extends Cross[JvmModule](scalaJVMVersions:_*)
  class JvmModule(val crossScalaVersion: String) extends UpickleModule with CommonJvmModule{
    def moduleDeps = Seq(ujson.jvm(), upack.jvm(), implicits.jvm())

    object test extends Tests with CommonBaseModule{
      def moduleDeps = {
        (super.moduleDeps :+ core.jvm().test) ++ (
          if (isDotty) Nil else Seq(
          ujson.argonaut(),
          ujson.circe(),
          ujson.json4s(),
          ujson.play(),
        ))
      }

      def scalacOptions = super.scalacOptions() ++ {
        if (isDotty) Seq("-Ximport-suggestion-timeout", "0")
        else Nil
      }
    }

    object testNonUtf8 extends Tests with CommonTestModule {
      def forkArgs = T {
        Seq("-Dfile.encoding=US-ASCII")
      }
    }
    object testSlow extends Tests with CommonTestModule{
      def moduleDeps = super.moduleDeps ++ Seq(JvmModule.this.test)
    }
  }

  object js extends Cross[JsModule](scalaJSVersions:_*)
  class JsModule(val crossScalaVersion: String, val crossScalaJSVersion: String) extends UpickleModule with CommonJsModule {
    def moduleDeps = Seq(
      ujson.js(crossScalaVersion, crossScalaJSVersion),
      upack.js(crossScalaVersion, crossScalaJSVersion),
      implicits.js(crossScalaVersion, crossScalaJSVersion)
    )

    object test extends Tests with CommonModule{
      def moduleDeps = super.moduleDeps ++ Seq(core.js(crossScalaVersion, crossScalaJSVersion).test)
    }
  }
  object native extends Cross[NativeModule](scalaNativeVersions:_*)
  class NativeModule(val crossScalaVersion: String, val crossScalaNativeVersion: String) extends UpickleModule with CommonNativeModule {
    def moduleDeps = Seq(
      ujson.native(crossScalaVersion, crossScalaNativeVersion),
      upack.native(crossScalaVersion, crossScalaNativeVersion),
      implicits.native(crossScalaVersion, crossScalaNativeVersion)
    )
    object test extends Tests with CommonTestModule{
      def moduleDeps = super.moduleDeps ++ Seq(core.native(crossScalaVersion, crossScalaNativeVersion).test)
      def allSourceFiles = T(super.allSourceFiles().filter(_.path.last != "DurationsTests.scala"))
    }
  }
}
def exampleJson = T.source(millSourcePath / "exampleJson")
trait BenchModule extends CommonModule{
  def scalaVersion = scala213
  def millSourcePath = build.millSourcePath / "bench"
  def ivyDeps = Agg(
    ivy"io.circe::circe-core::0.14.5",
    ivy"io.circe::circe-generic::0.14.5",
    ivy"io.circe::circe-parser::0.14.5",
    ivy"com.typesafe.play::play-json::2.9.4",
    ivy"io.argonaut::argonaut:6.2.6",
    ivy"org.json4s::json4s-ast:3.6.12",
    ivy"com.lihaoyi::sourcecode::$sourcecode",
  )
}

object bench extends Module {
  object js extends BenchModule with ScalaJSModule {
    def scalaJSVersion = scalaJSVersions.last._2
    def platformSegment = "js"
    def moduleDeps = Seq(upickle.js(scala213, scalaJS).test)
    def run(args: String*) = T.command {
      finalMainClassOpt() match{
        case Left(err) => mill.api.Result.Failure(err)
        case Right(_) =>
          ScalaJSWorkerApi.scalaJSWorker().run(
            scalaJSToolsClasspath().map(_.path),
            jsEnvConfig(),
            fullOpt().path.toIO
          )
          mill.api.Result.Success(())
      }
    }
  }

  object jvm extends BenchModule {
    def platformSegment = "jvm"
    def moduleDeps = Seq(upickle.jvm(scala213).test)
  }

  object native extends BenchModule with ScalaNativeModule {
    def platformSegment = "native"
    def scalaNativeVersion = scalaNative
    def moduleDeps = Seq(upickle.native(scala213, scalaNative).test)
    def ivyDeps = Agg(ivy"com.lihaoyi::sourcecode::$sourcecode")
    def allSourceFiles = T(super.allSourceFiles().filter(_.path.last != "NonNative.scala"))
    def releaseMode = ReleaseMode.ReleaseFast
    def nativeLTO = LTO.Thin
  }
}
