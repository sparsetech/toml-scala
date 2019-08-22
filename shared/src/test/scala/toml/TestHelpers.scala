package toml

import scala.meta.internal.fastparse.all._
import scala.meta.internal.fastparse.core.Parsed._

import org.scalatest.Matchers

object TestHelpers {
  import Matchers._

  def testSuccess(example: String, rules: Rules = Rules): Root =
    rules.root.parse(example) match {
      case Success(v, _)    => v
      case f: Failure[_, _] => fail(s"Failed to parse `$example`: ${f.msg}")
    }

  def testFailure(example: String, rules: Rules = Rules): Unit =
    rules.root.parse(example) match {
      case Success(_, _) => fail(s"Did not fail: $example")
      case _: Failure[_, _] =>
    }

  def shouldBeSuccess[T](r: Parsed[T]): Unit = r match {
    case s: Success[T, _, _] =>
    case f: Failure[_, _]    => fail(s"$r is not a Success: $f")
  }

  def shouldBeFailure[T](r: Parsed[T]): Unit = r match {
    case s: Success[T, _, _] => fail(s"$r is not a Failure.")
    case f: Failure[_, _]    =>
  }
}
