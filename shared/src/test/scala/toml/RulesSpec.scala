package toml

import org.scalatest.{FunSuite, Matchers}
import scala.meta.internal.fastparse.core.Parsed._

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

  test("Parse escaped table key identifier") {
    val example =
      """[a."tes\"t"]
        |b = 42
      """.stripMargin

    assert(testSuccess(example) == Root(List(
      Node.NamedTable(
        List("a", "tes\"t"),
        Map("b" -> Value.Num(42))))))
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

  test("Fail to parse non-TOML-compliant statement") {
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

  test("Parse floats with fractional part") {
    val example =
      """flt1 = +1.0
        |flt2 = 3.1415
        |flt3 = -0.01
      """.stripMargin

    assert(testSuccess(example) == Root(List(
      Node.Pair("flt1", Value.Real(+1.0)),
      Node.Pair("flt2", Value.Real(3.1415)),
      Node.Pair("flt3", Value.Real(-0.01)))))
  }

  test("Parse floats with exponent part") {
    val example =
      """flt4 = 5e+22
        |flt5 = 1e6
        |flt6 = -2E-2
      """.stripMargin

    assert(testSuccess(example) == Root(List(
      Node.Pair("flt4", Value.Real(5e+22)),
      Node.Pair("flt5", Value.Real(1e6)),
      Node.Pair("flt6", Value.Real(-2E-2)))))
  }

  test("Parse floats with fractional and exponent part") {
    val example = "flt7 = 6.626e-34\n" +
                  "poly = -45.321e12"

    assert(testSuccess(example) == Root(List(
      Node.Pair("flt7", Value.Real(6.626e-34)),
      Node.Pair("poly", Value.Real(-45.321e12)))))
  }

  test("Parse floats with underscores") {
    val example = "flt8 = 224_617.445_991_228"

    assert(testSuccess(example) == Root(List(
      Node.Pair("flt8", Value.Real(224617.445991228)))))
  }

  test("Parse infinity constant") {
    val example =
      """sf1 = inf
        |sf2 = +inf
        |sf3 = -inf
        |""".stripMargin

    assert(testSuccess(example) == Root(List(
      Node.Pair("sf1", Value.Real(Double.PositiveInfinity)),
      Node.Pair("sf2", Value.Real(Double.PositiveInfinity)),
      Node.Pair("sf3", Value.Real(Double.NegativeInfinity)))))
  }

  test("Parse NaN constant") {
    val example =
      """sf4 = nan
        |sf5 = +nan
        |sf6 = -nan
        |""".stripMargin

    val result = testSuccess(example).nodes
    assert(result.length == 3)

    // Cannot use `==` here since Double.NaN != Double.NaN
    assert(result.forall {
      case Node.Pair(_, Value.Real(v)) => v.equals(Double.NaN)
      case _ => false
    })
  }
}
