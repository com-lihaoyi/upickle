import mill._, mill.scalalib._, mill.scalalib.publish._, mill.scalajslib._


trait UpickleModule extends CrossScalaModule with PublishModule{

  def artifactName = "mill-" + super.artifactName()
  def publishVersion = "0.5.1"

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
  def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    ivy"com.lihaoyi::acyclic:0.1.5"
  )
  def compileIvyDeps = Agg(
    ivy"com.lihaoyi::acyclic:0.1.5",
    ivy"org.scala-lang:scala-reflect:${scalaVersion()}",
    ivy"org.scala-lang:scala-compiler:${scalaVersion()}"
  )
  def ivyDeps = Agg(
    ivy"com.lihaoyi::sourcecode::0.1.3"
  )
  def scalacOptions = Seq("-unchecked",
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
      val caseReader =
        if(i == 1) s"f(readJs[Tuple1[T1]](x)._1)"
        else s"f.tupled(readJs[Tuple$i[$typeTuple]](x))"
        s"""
        implicit def Tuple${i}Writer[$writerTypes]: TupleNWriter[Tuple$i[$typeTuple]] =
          TupleNWriter[Tuple$i[$typeTuple]](List($implicitWriterTuple), x => if (x == null) null else x.productIterator.toSeq)
        implicit def Tuple${i}Reader[$readerTypes]: TupleNReader[Tuple$i[$typeTuple]] =
            TupleNReader(List($implicitReaderTuple), x => Tuple$i($lookupTuple).asInstanceOf[Tuple$i[$typeTuple]])
        """
    }

    ammonite.ops.write(file, s"""
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
    Seq(PathRef(dir))
  }

}

trait UpickleTestModule extends TestModule{

  def ivyDeps = Agg(
    ivy"com.lihaoyi::utest::0.5.4",
    ivy"com.lihaoyi::acyclic:0.1.5"
  )

  def testFrameworks = Seq("utest.runner.Framework")
}

//object parserJs extends Cross[JawnJsModule]("2.11.11", "2.12.4")
//class JawnJsModule(val crossScalaVersion: String) extends UpickleModule{
//  def platformSegment = "js"
//  def millSourcePath = build.millSourcePath / "parser"
//  def generatedSources = Nil
//  def sources = T.sources(millSourcePath / "src" / "main")
//  object test extends Tests with UpickleTestModule{
//    def platformSegment = "js"
//    def millSourcePath = build.millSourcePath / "parser"
//  }
//}

object parserJvm extends Cross[JawnJvmModule]("2.11.11", "2.12.4")
class JawnJvmModule(val crossScalaVersion: String) extends UpickleModule{
  def millSourcePath = build.millSourcePath / "parser"
  def generatedSources = Nil
  object test extends Tests with UpickleTestModule{
  }
}

object upickleJvm extends Cross[UpickleJvmModule]("2.11.11", "2.12.4")
class UpickleJvmModule(val crossScalaVersion: String) extends UpickleModule{
  def millSourcePath = build.millSourcePath / "upickle"
  def moduleDeps = Seq(parserJvm())

  object test extends Tests with UpickleTestModule{

  }
}

//object upickleJs extends Cross[UpickleJsModule]("2.11.11", "2.12.4")
//class UpickleJsModule(val crossScalaVersion: String) extends UpickleModule with ScalaJSModule {
//  def moduleDeps = Seq(parserJs())
//  def platformSegment = "js"
//
//  def scalaJSVersion = "0.6.22"
//  def scalacOptions = T{
//    super.scalacOptions() ++ Seq({
//      val a = build.millSourcePath.toString.replaceFirst("[^/]+/?$", "")
//      val g = "https://raw.githubusercontent.com/lihaoyi/upickle"
//      s"-P:scalajs:mapSourceURI:$a->$g/v${publishVersion()}/"
//    })
//  }
//  object test extends Tests with UpickleTestModule{
//    def platformSegment = "js"
//    def millSourcePath = build.millSourcePath / "upickle"
//  }
//}

object test extends ScalaModule{
  def scalaVersion = "2.12.4"
  def moduleDeps = Seq(upickleJvm("2.12.4"))
  def sources = T.sources{millSourcePath}
}

