package toml

import Value._
import Node._

import org.scalatest.FunSuite

class ParseSpec extends FunSuite {
  test("Redefine value on root level") {
    val toml =
      """a = 23
        |a = 42
      """.stripMargin
    val result = Toml.parse(toml)
    assert(result == Left((List("a"), "Cannot redefine value")))
  }

  test("Redefine value in table") {
    val toml =
      """[a]
        |b = 23
        |b = 42
      """.stripMargin
    val result = Toml.parse(toml)
    assert(result == Left((List("a", "b"), "Cannot redefine value")))
  }

  test("Redefine value in array") {
    val toml =
      """[[a]]
        |b = 23
        |b = 42
      """.stripMargin
    val result = Toml.parse(toml)
    assert(result == Left((List("a", "b"), "Cannot redefine value")))
  }
}
