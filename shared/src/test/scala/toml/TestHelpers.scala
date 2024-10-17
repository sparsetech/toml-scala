package toml

import fastparse._
import fastparse.Parsed._

import org.scalatest.matchers.should.Matchers

object TestHelpers {
  import Matchers._

  def testSuccess(example: String, rules: Rules = Rules): Root =
    parse(example, rules.root(_), verboseFailures = true) match {
      case Success(v, _)    => v
      case f: Failure => fail(s"Failed to parse `$example`: ${f.longMsg}")
    }

  def testFailure(example: String, rules: Rules = Rules): Unit =
    parse(example,rules.root(_)) match {
      case Success(_, _) => fail(s"Did not fail: $example")
      case _: Failure =>
    }

  def shouldBeSuccess[T](r: Parsed[T]): Unit = r match {
    case s: Success[T] =>
    case f: Failure    => fail(s"$r is not a Success: $f")
  }

  def shouldBeFailure[T](r: Parsed[T]): Unit = r match {
    case s: Success[T] => fail(s"$r is not a Failure.")
    case f: Failure =>
  }
}
