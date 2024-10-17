package toml

import Value._
import Node._

import org.scalatest.funsuite.AnyFunSuite

class ParseSpec extends AnyFunSuite {
  test("Parse strings") {
    val toml =
      """
        |lines = '''
        |
        |The first newline is
        |trimmed in raw strings.
        |  All other whitespace
        |  is preserved.
        |'''
        |""".stripMargin

    val result = Toml.parse(toml)
    assert(result == Right(Tbl(Map("lines" -> Value.Str(
      "\n" +
      "The first newline is\n" +
      "trimmed in raw strings.\n" +
      "  All other whitespace\n" +
      "  is preserved.\n")))))
  }

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

  test("Extension: Parse inline tables with trailing comma") {
    val result = Toml.parse("""key = {
      a = 23,
      b = 42,
    }""", Set(Extension.MultiLineInlineTables))

    assert(result == Right(Tbl(
      Map("key" -> Tbl(Map("a" -> Num(23), "b" -> Num(42)))))))
  }
}
