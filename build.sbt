// Shadow sbt-scalajs' crossProject and CrossType until Scala.js 1.0.0 is released
import sbtcrossproject.{crossProject, CrossType}

val Scala2_11       = "2.11.12"
val Scala2_12       = "2.12.4"
val FastParse       = "1.0.0"
val Shapeless       = "2.3.2"
val ShapelessNative = "2.3.3-pre-1"
val Paradise        = "2.1.1"
val ScalaCheck      = "1.13.5"
val ScalaTest       = "3.0.4"

val SharedSettings = Seq(
  name         := "toml-scala",
  organization := "tech.sparse",

  scalaVersion       := Scala2_12,
  crossScalaVersions := Seq(Scala2_12, Scala2_11),

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

enablePlugins(ScalaNativePlugin)

lazy val root = project.in(file("."))
  .aggregate(tomlJS, tomlJVM, tomlNative)
  .settings(SharedSettings: _*)
  .settings(skip in publish := false)

lazy val toml =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .in(file("."))
    .settings(SharedSettings)
    .settings(
      libraryDependencies ++= Seq(
        "com.lihaoyi" %%% "fastparse" % FastParse,
        compilerPlugin("org.scalamacros" % "paradise" % Paradise cross CrossVersion.full)
      )
    )
    .jsSettings(
      libraryDependencies ++= Vector(
        "com.chuusai"    %%% "shapeless"  % Shapeless,
        "org.scalacheck" %%% "scalacheck" % ScalaCheck % "test",
        "org.scalatest"  %%% "scalatest"  % ScalaTest  % "test"
      )
    )
    .jvmSettings(
      libraryDependencies ++= Vector(
        "com.chuusai"    %% "shapeless"  % Shapeless,
        "org.scalacheck" %% "scalacheck" % ScalaCheck % "test",
        "org.scalatest"  %% "scalatest"  % ScalaTest  % "test"
      )
    )
    .nativeSettings(
      scalaVersion       := Scala2_11,
      crossScalaVersions := Seq(Scala2_11),
      libraryDependencies ++= Vector(
        "com.github.alexarchambault" %%% "shapeless" % ShapelessNative
      ),
      excludeFilter in Test := "*"
    )

lazy val tomlJS     = toml.js
lazy val tomlJVM    = toml.jvm
lazy val tomlNative = toml.native
