import sbt._
import Keys._
import scala.scalajs.sbtplugin.ScalaJSPlugin.ScalaJSKeys.scalaJSEnvironment
import scala.scalajs.sbtplugin.ScalaJSPlugin.scalaJSSettings
object Build extends sbt.Build{
  lazy val js = project.in(file("js"))
                       .settings(sharedSettings ++ scalaJSSettings:_*)
                       .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang.modules.scalajs" %% "scalajs-dom" % "0.3",
      "com.lihaoyi" %% "utest" % "0.1.3-JS"
    ),
    (loadedTestFrameworks in Test) := {
      (loadedTestFrameworks in Test).value.updated(
        sbt.TestFramework(classOf[utest.runner.JsFramework].getName),
        new utest.runner.JsFramework(environment = (scalaJSEnvironment in Test).value)
      )
    },
    version := version.value + "-JS"
  )
  lazy val jvm = project.in(file("jvm"))
                        .settings(sharedSettings:_*)
                        .settings(
    libraryDependencies ++= Seq(
      "org.parboiled" %% "parboiled" % "2.0-M2",
      "com.lihaoyi" %% "utest" % "0.1.3"
    ),
    testFrameworks += new TestFramework("utest.runner.JvmFramework")
  )

  val sharedSettings = Seq(
    organization := "com.lihaoyi",
    crossScalaVersions := Seq("2.10.4", "2.11.0-RC4"),
    scalaVersion := "2.10.4",
    version := "0.1.0",
    name := "picklite",

    unmanagedSourceDirectories in Compile <+= baseDirectory(_ / ".." / "shared" / "main" / "scala"),
    unmanagedSourceDirectories in Test <+= baseDirectory(_ / ".." / "shared" / "test" / "scala"),

    // Sonatype
    publishArtifact in Test := false,
    publishTo <<= version { (v: String) =>
      Some("releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
    },

    pomExtra := (
      <url>https://github.com/lihaoyi/utest</url>
        <licenses>
          <license>
            <name>MIT license</name>
            <url>http://www.opensource.org/licenses/mit-license.php</url>
          </license>
        </licenses>
        <scm>
          <url>git://github.com/lihaoyi/utest.git</url>
          <connection>scm:git://github.com/lihaoyi/utest.git</connection>
        </scm>
        <developers>
          <developer>
            <id>lihaoyi</id>
            <name>Li Haoyi</name>
            <url>https://github.com/lihaoyi</url>
          </developer>
        </developers>
      )
  )
}

