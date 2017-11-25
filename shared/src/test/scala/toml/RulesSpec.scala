package toml

import org.scalatest.{FunSuite, Matchers}
import fastparse.core.Parsed._

class RulesSpec extends FunSuite with Matchers {
  import TestHelpers._

  val smallTest =
    """author = "Anonymous"
      |
      |[num."theory"]
      |state   = false
      |numbers = [23, 48]""".stripMargin

  test("Parse sample") {
    testSuccess(smallTest)
  }

  test("Strings in table headers") {
    assert(
      testSuccess("[ j . \"ʞ\" . 'l' ]") ==
      Root(List(Node.NamedTable(List("j", "ʞ", "l"), Map.empty))))
  }

  test("Parse quoted strings") {
    val toml = """a = "I'm a string. \"You can quote me\". Name\tJos\u00E9\nLocation\tSF.""""
    val nodes = testSuccess(toml).nodes
    assert(nodes(0) == Node.Pair("a", Value.Str(
      "I'm a string. \"You can quote me\". Name\tJosé\nLocation\tSF.")))
  }

  test("Parse unicode strings") {
    val uc1 = "\\" + "u0107"
    val uc2 = "\\" + "U0000004D"

    val toml =
      s"""str1 = "t$uc1"
        |str2 = "$uc2"
      """.stripMargin

    val nodes = testSuccess(toml).nodes
    assert(nodes(0) == Node.Pair("str1", Value.Str("tć")))
    assert(nodes(1) == Node.Pair("str2", Value.Str("M")))
  }

  test("Parse empty comments") {
    val str = "#\n[test]"
    testSuccess(str)

    val str2 = "#"
    testSuccess(str2)

    val str3 = "#\n"
    testSuccess(str3)
  }

  test("Parse escaped double quotes inside a string") {
    val example =
      """
        |harder_test_string = " And when \"'s are in the string, along with # \""   # "and comments are there too"
      """.stripMargin
    testSuccess(example)
  }

  test("Parse inline tables") {
    val example =
      """
        |a = { name = "value", name2 = "value2" }
        |b = { name = "value",
        |      # Trailing comma
        |    }
      """.stripMargin
    testSuccess(example)
  }

  test("Parse complex table keys") {
    val example =
      """[asdf."bit#"]
        |"hello" = "asdfasdf"
      """.stripMargin
    testSuccess(example)
  }

  test("Parse multi-line array with trailing commas") {
    val example =
      """
        |multi_line_array = [
        |    "]",
        |    # ] Comment
        |    ]
      """.stripMargin
    testSuccess(example)
  }

  test("Fail to parse non-toml-compliant statement") {
    val example = "[error]   if you didn't catch this, your parser is broken"
    testFailure(example)
  }

  test("Fail to parse comment at EOL") {
    val example = "string = \"Anything other than tabs, spaces and newline after a keygroup or key value pair has ended should produce an error unless it is a comment\"   like this"
    testFailure(example)
  }

  test("Fail to parse end of comment after tricky array declaration") {
    val example =
      """array = [
        |         "This might most likely happen in multiline arrays",
        |         Like here,
        |         "or here,
        |         and here"
        |]     End of array comment, forgot the #
      """.stripMargin
    testFailure(example)
  }

  test("Fail to parse comment at the end of key-pair definition") {
    val example = "number = 3.14  p"
    testFailure(example)
  }
}
