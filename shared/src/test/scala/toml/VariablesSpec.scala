package toml

import org.scalatest.{FunSuite, Matchers}
import scala.meta.internal.fastparse.core.Parsed._

class VariablesSpec extends FunSuite with Matchers {
  import TestHelpers._

  // TODO update README

  val scalaTest  = Value.Arr(List("org.scalatest", "scalatest", "3.0.8").map(Value.Str(_)))
  val scalaCheck = Value.Arr(List("org.scalacheck", "scalacheck", "1.14.0").map(Value.Str(_)))

  test("Define variables") {
    val example =
      """
        |$a = 42
        |$b = "hello"
        |$c = true
      """.stripMargin

    testFailure(example)
    testSuccess(example, new Rules(Set(Extension.Variables)))

    val result = Toml.parse(example, Set(Extension.Variables))
    assert(result == Right(Value.Tbl(Map())))
  }

  test("Assign variables") {
    val example =
      """
        |$a = 42
        |$b = "hello"
        |$c = true
        |
        |a = $a
        |b = $b
        |c = $c
      """.stripMargin

    testFailure(example)
    testSuccess(example, new Rules(Set(Extension.Variables)))

    val result = Toml.parse(example, Set(Extension.Variables))
    assert(result == Right(Value.Tbl(Map(
      "a" -> Value.Num(42),
      "b" -> Value.Str("hello"),
      "c" -> Value.Bool(true)
    ))))
  }

  test("Assign variable to variable") {
    val example =
      """
        |$a = 42
        |$b = $a
        |
        |a = $b
      """.stripMargin

    testFailure(example)
    testSuccess(example, new Rules(Set(Extension.Variables)))

    val result = Toml.parse(example, Set(Extension.Variables))
    assert(result == Right(Value.Tbl(Map(
      "a" -> Value.Num(42)
    ))))
  }

  test("Assign variable to variable (2)") {
    val example =
      """
        |$a = 42
        |$b = [$a]
        |
        |a = $b
      """.stripMargin

    testFailure(example)
    testSuccess(example, new Rules(Set(Extension.Variables)))

    val result = Toml.parse(example, Set(Extension.Variables))
    assert(result == Right(Value.Tbl(Map(
      "a" -> Value.Arr(List(
        Value.Num(42)
    ))))))
  }

  test("Redefine variable") {
    val example =
      """
        |$a = 23
        |$a = 42
      """.stripMargin

    testFailure(example)
    testSuccess(example, new Rules(Set(Extension.Variables)))

    val result = Toml.parse(example, Set(Extension.Variables))
    assert(result == Left(List() -> "Cannot redefine variable `a`"))
  }

  test("Access undeclared variable") {
    val example =
      """
        |a = $b
      """.stripMargin

    testFailure(example)
    testSuccess(example, new Rules(Set(Extension.Variables)))

    val result = Toml.parse(example, Set(Extension.Variables))
    assert(result == Left(List("a") -> "Variable `b` not declared"))
  }

  test("Access variables in list") {
    val example =
      """
        |$scalaTest  = ["org.scalatest" , "scalatest" , "3.0.8" ]
        |$scalaCheck = ["org.scalacheck", "scalacheck", "1.14.0"]
        |[a]
        |scalaDeps = [$scalaTest, $scalaCheck]
        |[b]
        |scalaDeps = [$scalaTest]
      """.stripMargin

    testFailure(example)
    testSuccess(example, new Rules(Set(Extension.Variables)))

    val result = Toml.parse(example, Set(Extension.Variables))
    assert(result == Right(Value.Tbl(Map(
      "a" -> Value.Tbl(Map("scalaDeps" -> Value.Arr(List(scalaTest, scalaCheck)))),
      "b" -> Value.Tbl(Map("scalaDeps" -> Value.Arr(List(scalaTest))))
    ))))
  }

  test("Define variables in different scope") {
    // Variables are always defined in the global scope
    val example =
      """
        |[a]
        |$scalaTest  = ["org.scalatest" , "scalatest" , "3.0.8" ]
        |$scalaCheck = ["org.scalacheck", "scalacheck", "1.14.0"]
        |scalaDeps = [$scalaTest, $scalaCheck]
        |[b]
        |scalaDeps = [$scalaTest]
      """.stripMargin

    testFailure(example)
    testSuccess(example, new Rules(Set(Extension.Variables)))

    val result = Toml.parse(example, Set(Extension.Variables))
    assert(result == Right(Value.Tbl(Map(
      "a" -> Value.Tbl(Map("scalaDeps" -> Value.Arr(List(scalaTest, scalaCheck)))),
      "b" -> Value.Tbl(Map("scalaDeps" -> Value.Arr(List(scalaTest))))
    ))))
  }

  test("Access variables in nested table") {
    val example =
      """
        |$scalaTest  = ["org.scalatest" , "scalatest" , "3.0.8" ]
        |$scalaCheck = ["org.scalacheck", "scalacheck", "1.14.0"]
        |a = [{ b = [[$scalaTest]] }]
      """.stripMargin

    testFailure(example)
    testSuccess(example, new Rules(Set(Extension.Variables)))

    val result = Toml.parse(example, Set(Extension.Variables))

    assert(result == Right(Value.Tbl(Map(
      "a" -> Value.Arr(List(Value.Tbl(Map(
        "b" -> Value.Arr(List(
          Value.Arr(List(scalaTest))))))))))))
  }

  test("Concatenate strings") {
    val example =
      """
        |$fontStack     = "Helvetica, sans-serif"
        |$primaryColour = "#333"
        |
        |[body]
        |font   = "100% " + $fontStack
        |colour = $primaryColour
      """.stripMargin

    testFailure(example)
    testSuccess(example, new Rules(Set(Extension.Variables)))

    val result = Toml.parse(example, Set(Extension.Variables))
    assert(result == Right(Value.Tbl(Map(
      "body" -> Value.Tbl(Map(
        "font"   -> Value.Str("100% Helvetica, sans-serif"),
        "colour" -> Value.Str("#333")))))))
  }

  test("Concatenate arrays") {
    val example =
      """
        |$success = [200, 201, 202]
        |$error   = [400, 401, 403]
        |codes    = $success + $error
      """.stripMargin

    testFailure(example)
    testSuccess(example, new Rules(Set(Extension.Variables)))

    val result = Toml.parse(example, Set(Extension.Variables))
    assert(result == Right(Value.Tbl(Map(
      "codes" -> Value.Arr(List(200, 201, 202, 400, 401, 403).map(Value.Num(_)))))))
  }

  test("Concatenate numbers") {
    // For the time being, + is strictly meant for concatenation
    // Arithmetic is not supported to avoid having to add other operators
    // as well
    val example =
      """
        |$ok       = 200
        |$notFound = 404
        |code = $ok + $notFound
      """.stripMargin

    testFailure(example)
    testSuccess(example, new Rules(Set(Extension.Variables)))

    val result = Toml.parse(example, Set(Extension.Variables))
    assert(result == Left(List() -> "Cannot concatenate Num values"))
  }

  test("Concatenate booleans") {
    val example =
      """
        |$user  = true
        |$admin = true
        |acl = $user + $admin
      """.stripMargin

    testFailure(example)
    testSuccess(example, new Rules(Set(Extension.Variables)))

    val result = Toml.parse(example, Set(Extension.Variables))
    assert(result == Left(List() -> "Cannot concatenate Bool values"))
  }
}
