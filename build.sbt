// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val Scala2_11  = "2.11.12"
val Scala2_12  = "2.12.8"
val FastParse  = "1.0.0"
val Shapeless  = "2.3.3"
val ScalaCheck = "1.14.0"
val ScalaTest  = "3.0.5"
val ScalaTestNative = "3.2.0-SNAP10"

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

lazy val root = project.in(file("."))
  .aggregate(toml.js, toml.jvm, toml.native)
  .settings(SharedSettings: _*)
  .settings(skip in publish := true)

lazy val toml =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .in(file("."))
    .settings(SharedSettings)
    .settings(
      libraryDependencies ++= Seq(
        "com.lihaoyi" %%% "fastparse" % FastParse,
        "com.chuusai" %%% "shapeless" % Shapeless
      )
    )
    .jsSettings(
      libraryDependencies ++= Vector(
        "org.scalacheck" %%% "scalacheck" % ScalaCheck % "test",
        "org.scalatest"  %%% "scalatest"  % ScalaTest  % "test"
      )
    )
    .jvmSettings(
      libraryDependencies ++= Vector(
        "org.scalacheck" %% "scalacheck" % ScalaCheck % "test",
        "org.scalatest"  %% "scalatest"  % ScalaTest  % "test"
      )
    )
    .nativeSettings(
      scalaVersion       := Scala2_11,
      crossScalaVersions := Seq(Scala2_11),
      // See https://github.com/scalalandio/chimney/issues/78#issuecomment-419705142
      nativeLinkStubs    := true,
      libraryDependencies ++= Vector(
        "org.scalatest" %%% "scalatest" % ScalaTestNative  % "test"
      ),
      excludeFilter in Test := "*GeneratedSpec*" || "*Generators*"
    )
