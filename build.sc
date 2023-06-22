// plugins
import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version::0.4.0`
import $ivy.`com.github.lolgab::mill-mima::0.0.23`

// imports
import mill._
import mill.define.Target
import mill.scalalib._
import mill.scalalib.publish._
import mill.scalajslib._
import mill.scalanativelib._
import mill.scalalib.api.ZincWorkerUtil.isScala3
import mill.scalanativelib.api.{LTO, ReleaseMode}
import de.tobiasroeser.mill.vcs.version.VcsVersion
import com.github.lolgab.mill.mima._

val scala212  = "2.12.17"
val scala213  = "2.13.10"

val scala3   = "3.2.2"
val scalaNative = "0.4.10"
val acyclic = "0.3.8"
val sourcecode = "0.3.0"

val dottyCustomVersion = Option(sys.props("dottyVersion"))

val scala2JVMVersions = Seq(scala212, scala213)
val scalaVersions = scala2JVMVersions ++ Seq(scala3) ++ dottyCustomVersion

trait CommonPlatformModule extends ScalaModule with PlatformScalaModule{

  def sources = T.sources {
    super.sources() ++
    Option.when(scalaVersion() != scala212)(PathRef(millSourcePath / "src-2.13+")) ++
    (platformScalaSuffix match {
      case "jvm" => Seq(PathRef(millSourcePath / "src-jvm-native"))
      case "native" => Seq(PathRef(millSourcePath / "src-js-native"), PathRef(millSourcePath / "src-jvm-native"))
      case "js" => Seq(PathRef(millSourcePath / "src-js-native"))
    })
  }
}

trait CommonPublishModule
  extends ScalaModule with PublishModule with Mima with CrossScalaModule { outer =>

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

  def templates = T.sources {
    for(src <- sources()) yield{
      val s"src$rest" = src.path.last
      PathRef(src.path / os.up / s"templates$rest")
    }
  }
  def generatedSources = T{
    for{
      pathRef <- templates()
      p <- if (os.exists(pathRef.path)) os.list(pathRef.path) else Nil
      rename <- Seq("Char", "Byte")
    }{
      def replace(s: String) = s.replace("Elem", rename).replace("elem", rename.toLowerCase)
      os.write(T.dest / replace(p.last), replace(os.read(p)))
    }

    Seq(PathRef(T.dest))
  }

  def scalacOptions = T {
    Seq("-unchecked", "-deprecation", "-encoding", "utf8", "-feature", "-Xfatal-warnings") ++
    Agg.when(!isScala3(scalaVersion()))("-opt:l:method").toSeq
  }

  trait CommonTestModule0 extends ScalaModule with TestModule.Utest {
    def ivyDeps = {
      Agg(ivy"com.lihaoyi::utest::0.8.1") ++
      Option.when(!isScala3(scalaVersion()))(ivy"com.lihaoyi:::acyclic:$acyclic")
    }

    def scalacOptions = super.scalacOptions() ++
      Agg.when(isScala3(scalaVersion())) (
        "-Ximplicit-search-limit",
        "200000"
      )
  }
}

trait CommonJvmModule extends CommonPublishModule with CommonPlatformModule{
  trait CommonTestModule extends ScalaTests with CommonTestModule0
}

trait CommonJsModule extends CommonPublishModule with ScalaJSModule with CommonPlatformModule {
  def scalaJSVersion = "1.13.0"

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

  trait CommonTestModule extends ScalaJSTests with CommonTestModule0
}

trait CommonNativeModule extends CommonPublishModule with ScalaNativeModule with CommonPlatformModule {
  def scalaNativeVersion = "0.4.10"

  trait CommonTestModule extends ScalaNativeTests with CommonTestModule0
}

object upack extends Module {
  object js extends Cross[JsModule](scalaVersions)
  trait JsModule extends CommonJsModule {
    def moduleDeps = Seq(upickle.core.js())

    object test extends CommonTestModule{
      def moduleDeps = super.moduleDeps ++ Seq(ujson.js().test, upickle.core.js().test)
    }
  }

  object jvm extends Cross[JvmModule](scalaVersions)
  trait JvmModule extends CommonJvmModule {
    def moduleDeps = Seq(upickle.core.jvm())
    object test extends CommonTestModule{
      def moduleDeps = super.moduleDeps ++ Seq(ujson.jvm().test, upickle.core.jvm().test)
    }
  }

  object native extends Cross[NativeModule](scalaVersions)
  trait NativeModule extends CommonNativeModule {
    def moduleDeps = Seq(upickle.core.native())

    object test extends CommonTestModule{
      def moduleDeps = super.moduleDeps ++ Seq(ujson.native().test, upickle.core.native().test)
    }
  }
}

object ujson extends Module{
  object js extends Cross[JsModule](scalaVersions)
  trait JsModule extends CommonJsModule {
    def moduleDeps = Seq(upickle.core.js())

    object test extends CommonTestModule{
      def moduleDeps = super.moduleDeps ++ Seq(upickle.core.js().test)
    }
  }

  object jvm extends Cross[JvmModule](scalaVersions)
  trait JvmModule extends CommonJvmModule{
    def moduleDeps = Seq(upickle.core.jvm())

    object test extends CommonTestModule{
      def moduleDeps = super.moduleDeps ++ Seq(upickle.core.jvm().test)
    }
  }

  object native extends Cross[NativeModule](scalaVersions)
  trait NativeModule extends CommonNativeModule {
    def moduleDeps = Seq(upickle.core.native())

    object test extends CommonTestModule{
      def moduleDeps = super.moduleDeps ++ Seq(upickle.core.native().test)
    }
  }

  object argonaut extends Cross[ArgonautModule](scala2JVMVersions)
  trait ArgonautModule extends CommonPublishModule{
    def moduleDeps = Seq(ujson.jvm())
    def ivyDeps = Agg(ivy"io.argonaut::argonaut:6.2.6")
  }

  object json4s extends Cross[Json4sModule](scalaVersions)
  trait Json4sModule extends CommonPublishModule{
    def moduleDeps = Seq(ujson.jvm())
    def ivyDeps = Agg(
      ivy"org.json4s::json4s-ast:4.0.6",
      ivy"org.json4s::json4s-native:4.0.6"
    )
  }

  object circe extends Cross[CirceModule](scalaVersions)
  trait CirceModule extends CommonPublishModule{
    def moduleDeps = Seq(ujson.jvm())
    val circeVersion = "0.14.5"
    def ivyDeps = Agg(ivy"io.circe::circe-parser:$circeVersion")
  }

  object play extends Cross[PlayModule](scala2JVMVersions)
  trait PlayModule extends CommonPublishModule{
    def moduleDeps = Seq(ujson.jvm())
    val playJsonVersion = "2.9.4"
    def ivyDeps = Agg(ivy"com.typesafe.play::play-json:$playJsonVersion")
  }
}

object upickle extends Module{  
  object core extends Module {
    trait CommonCoreModule extends CommonPublishModule {
      def ivyDeps = Agg(ivy"com.lihaoyi::geny::1.0.0")
    }

    object js extends Cross[CoreJsModule](scalaVersions)
    trait CoreJsModule extends CommonJsModule with CommonCoreModule {
      object test extends CommonTestModule
    }

    object jvm extends Cross[CoreJvmModule](scalaVersions)
    trait CoreJvmModule extends CommonJvmModule with CommonCoreModule{
      object test extends CommonTestModule
    }

    object native extends Cross[CoreNativeModule](scalaVersions)
    trait CoreNativeModule extends CommonNativeModule with CommonCoreModule {
      object test extends CommonTestModule
    }
  }

  object implicits extends Module {
    trait ImplicitsModule extends CommonPublishModule{
      def compileIvyDeps = T{
        Agg.when(!isDotty)(
          ivy"com.lihaoyi:::acyclic:$acyclic",
          ivy"org.scala-lang:scala-reflect:${scalaVersion()}"
        )
      }

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

    object js extends Cross[JsModule](scalaVersions)
    trait JsModule extends ImplicitsModule with CommonJsModule {
      def moduleDeps = Seq(core.js())

      object test extends CommonTestModule{
        def moduleDeps = super.moduleDeps ++ Seq(ujson.js().test, core.js().test)
      }
    }

    object jvm extends Cross[JvmModule](scalaVersions)
    trait JvmModule extends ImplicitsModule with CommonJvmModule{
      def moduleDeps = Seq(core.jvm())

      object test extends CommonTestModule{
        def moduleDeps = super.moduleDeps ++ Seq(ujson.jvm().test, core.jvm().test)
      }
    }

    object native extends Cross[NativeModule](scalaVersions)
    trait NativeModule extends ImplicitsModule with CommonNativeModule {
      def moduleDeps = Seq(core.native())

      object test extends CommonTestModule{
        def moduleDeps = super.moduleDeps ++ Seq(ujson.native().test, core.native().test)
      }
    }
  }

  trait UpickleModule extends CommonPublishModule {
    def compileIvyDeps =
      Agg.when(!isDotty)(
        ivy"com.lihaoyi:::acyclic:$acyclic",
        ivy"org.scala-lang:scala-reflect:${scalaVersion()}",
        ivy"org.scala-lang:scala-compiler:${scalaVersion()}"
      )
  }

  object jvm extends Cross[JvmModule](scalaVersions)
  trait JvmModule extends UpickleModule with CommonJvmModule{
    def moduleDeps = Seq(ujson.jvm(), upack.jvm(), implicits.jvm())

    object test extends CommonTestModule{
      def moduleDeps =
        super.moduleDeps ++
        Seq(core.jvm().test) ++
        (
          if (isDotty) Nil
          else Seq(ujson.argonaut(), ujson.circe(), ujson.json4s(), ujson.play())
        )
    }

    object testNonUtf8 extends CommonTestModule {
      def forkArgs = Seq("-Dfile.encoding=US-ASCII")
    }

    object testSlow extends CommonTestModule{
      def moduleDeps = super.moduleDeps ++ Seq(JvmModule.this.test)
    }
  }

  object js extends Cross[JsModule](scalaVersions)
  trait JsModule extends UpickleModule with CommonJsModule {
    def moduleDeps = Seq(ujson.js(), upack.js(), implicits.js())

    object test extends CommonTestModule{
      def moduleDeps = super.moduleDeps ++ Seq(core.js().test)
    }
  }

  object native extends Cross[NativeModule](scalaVersions)
  trait NativeModule extends UpickleModule with CommonNativeModule {
    def moduleDeps = Seq(ujson.native(), upack.native(), implicits.native())

    object test extends CommonTestModule{
      def moduleDeps = super.moduleDeps ++ Seq(core.native().test)
      def allSourceFiles = T(super.allSourceFiles().filter(_.path.last != "DurationsTests.scala"))
    }
  }
}

def exampleJson = T.source(millSourcePath / "exampleJson")

trait BenchModule extends CommonPlatformModule{
  def scalaVersion = scala213
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
    def scalaJSVersion = "1.13.0"
    def moduleDeps = Seq(upickle.js(scala213).test)
  }

  object jvm extends BenchModule {
    def moduleDeps = Seq(upickle.jvm(scala213).test)
  }

  object native extends BenchModule with ScalaNativeModule {
    def scalaNativeVersion = scalaNative
    def moduleDeps = Seq(upickle.native(scala213).test)
    def ivyDeps = Agg(ivy"com.lihaoyi::sourcecode::$sourcecode")
    def allSourceFiles = T(super.allSourceFiles().filter(_.path.last != "NonNative.scala"))
    def releaseMode = ReleaseMode.ReleaseFast
    def nativeLTO = LTO.Thin
  }
}
