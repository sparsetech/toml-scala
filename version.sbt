import scala.sys.process._
import scala.util.Try

ThisBuild / version := {
  val current = (ThisBuild / version).value
  val v =
    Try(Seq("git", "describe", "--tags").!!.trim.tail)
      .getOrElse(current)
  println(s"[info] Setting version to: $v")
  v
}
