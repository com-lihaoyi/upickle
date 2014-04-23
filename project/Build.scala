import sbt._
import Keys._
import scala.scalajs.sbtplugin.ScalaJSPlugin.ScalaJSKeys.scalaJSEnvironment
import scala.scalajs.sbtplugin.ScalaJSPlugin.scalaJSSettings

class CrossCrossBuild(sharedSettings: Seq[Def.Setting[_]]){
  val defaultSettings = Seq(
    unmanagedSourceDirectories in Compile <+= baseDirectory(_ / ".." / "shared" / "main" / "scala"),
    unmanagedSourceDirectories in Test <+= baseDirectory(_ / ".." / "shared" / "test" / "scala")
  )
  lazy val js = project.in(file("js"))
                       .settings(sharedSettings ++ scalaJSSettings ++ defaultSettings: _*)
                       .settings(
      libraryDependencies ++= Seq(
        "com.lihaoyi" %% "utest" % "0.1.3-JS" % "test"
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
                        .settings(sharedSettings ++ defaultSettings:_*)
                        .settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "utest" % "0.1.3" % "test"
    ),
    testFrameworks += new TestFramework("utest.runner.JvmFramework")
  )

  lazy val root = project.in(file("."))
                         .aggregate(js, jvm)
                         .settings(

    crossScalaVersions := Seq("2.10.4", "2.11.0"),
    scalaVersion := "2.10.4"
  )
}

object Build extends sbt.Build{
  val cross = new CrossCrossBuild(
    Seq(
      organization := "com.lihaoyi",

      version := "0.1.0",
      name := "upickle",

      // Sonatype
      publishArtifact in Test := false,
      publishTo <<= version { (v: String) =>
        Some("releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
      },

      pomExtra := (
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
    )
  )

  lazy val root = cross.root

  lazy val js = cross.js

  lazy val jvm = cross.jvm
}

