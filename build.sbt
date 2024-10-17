// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val Scala2_12  = "2.12.19"
val Scala2_13  = "2.13.14"
val FastParse  = "3.1.1"
val Shapeless  = "2.3.12"
val ScalaCheck = "1.18.1"
val ScalaTest  = "3.2.19"

val ScalaTestScalaCheck  = s"$ScalaTest.0"

val SharedSettings = Seq(
  name         := "toml-scala",
  organization := "tech.sparse",

  scalaVersion       := Scala2_13,
  crossScalaVersions := Seq(Scala2_13, Scala2_12),

  pomExtra :=
    <url>https://github.com/sparsetech/toml-scala</url>
    <licenses>
      <license>
        <name>MPL-2.0 License</name>
        <url>https://opensource.org/licenses/MPL-2.0</url>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:sparsetech/toml-scala.git</url>
    </scm>
    <developers>
      <developer>
        <id>tindzk</id>
        <name>Tim Nieradzik</name>
        <url>http://github.com/tindzk</url>
      </developer>
    </developers>
)

lazy val root = project.in(file("."))
  .aggregate(toml.js, toml.jvm, toml.native)
  .settings(SharedSettings: _*)
  .settings(publish / skip := true)

lazy val toml =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .in(file("."))
    .settings(SharedSettings)
    .settings(
      libraryDependencies ++= Seq(
        "com.lihaoyi" %%% "fastparse" % FastParse,
        "com.chuusai"   %%% "shapeless" % Shapeless,
        "org.scalacheck" %%% "scalacheck" % ScalaCheck % Test,
        "org.scalatest"  %%% "scalatest"  % ScalaTest  % Test,
        "org.scalatestplus" %%% s"scalacheck-${ScalaCheck.split('.').take(2).mkString("-")}" % ScalaTestScalaCheck % Test
      )
    )
