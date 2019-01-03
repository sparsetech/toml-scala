package toml

import Value._
import Node._

import org.scalatest.FunSuite

class GenerationSpec extends FunSuite {
  def check(root: Root, expected: String): Unit = {
    val generated = Toml.generate(root)
    val parsed = Toml.parse(generated)

    assert(parsed == Right(Embed.root(root)))
    assert(generated == expected)
  }

  test("Empty list") {
    val root = Root(List(Pair("a", Arr(List()))))
    val pairs = "a = []"
    check(root, pairs)
  }

  test("Two pairs") {
    val root = Root(List(
      Pair("b", Num(2)),
      Pair("a", Num(1))))

    val pairs =
      """b = 2
        |a = 1
      """.stripMargin.trim

    check(root, pairs)
  }

  test("Table") {
    val root = Root(List(NamedTable(List("table"), Map("a" -> Num(1)))))

    val table =
      """
        |[table]
        |a = 1
      """.stripMargin.trim

    check(root, table)
  }

  test("Escape table reference") {
    val root = Root(List(NamedTable(List("a", "b 2"), Map("a" -> Num(1)))))

    val table =
      """
        |[a."b 2"]
        |a = 1
      """.stripMargin.trim

    check(root, table)
  }

  test("Escape table reference (2)") {
    val root = Root(List(NamedTable(List("a", "b\"c"), Map("a" -> Num(1)))))

    val table =
      """
        |[a."b\"c"]
        |a = 1
      """.stripMargin.trim

    check(root, table)
  }

  test("Escape table reference (3)") {
    val root = Root(List(NamedTable(List("a", "b.c"), Map("a" -> Num(1)))))

    val table =
      """
        |[a."b.c"]
        |a = 1
      """.stripMargin.trim

    check(root, table)
  }

  test("Pair and table") {
    val root = Root(List(
      Pair("a", Num(1)),
      NamedTable(
        List("table"), Map("b" -> Num(2)))))

    val table =
      """
        |a = 1
        |
        |[table]
        |b = 2
      """.stripMargin.trim

    check(root, table)
  }

  test("Two tables") {
    val root = Root(List(
      Pair("a", Num(1)),
      NamedTable(List("table"), Map("b" -> Num(2))),
      NamedTable(List("table2"), Map("c" -> Num(3)))))

    val table =
      """
        |a = 1
        |
        |[table]
        |b = 2
        |
        |[table2]
        |c = 3
      """.stripMargin.trim

    check(root, table)
  }

  test("Nested table") {
    val root = Root(List(
      NamedTable(List("table", "table2"), Map("value" -> Num(42)))))

    val table =
      """
        |[table.table2]
        |value = 42
      """.stripMargin.trim

    check(root, table)
  }

  test("Regular list") {
    val root = Root(List(Pair("scalaOptions", Arr(List(
      "-encoding", "UTF-8",
      "-unchecked",
      "-deprecation",
      "-Xfuture",
      "-Yno-adapted-args",
      "-Ywarn-numeric-widen",
      "-feature"
    ).map(Str)))))

    val table =
      """
        |scalaOptions = [
        |  "-encoding",
        |  "UTF-8",
        |  "-unchecked",
        |  "-deprecation",
        |  "-Xfuture",
        |  "-Yno-adapted-args",
        |  "-Ywarn-numeric-widen",
        |  "-feature"
        |]
      """.stripMargin.trim

    check(root, table)
  }

  test("Nested list") {
    val root = Root(List(Pair("scalaDeps", Arr(List(
      Arr(List(Str("io.monix"), Str("minitest"), Str("2.2.2"))),
      Arr(List(Str("org.scalacheck"), Str("scalacheck"), Str("1.14.0"))),
      Arr(List(Str("org.scalatest"), Str("scalatest"), Str("3.2.0-SNAP10")))
    )))))

    val table =
      """
        |scalaDeps = [
        |  ["io.monix", "minitest", "2.2.2"],
        |  ["org.scalacheck", "scalacheck", "1.14.0"],
        |  ["org.scalatest", "scalatest", "3.2.0-SNAP10"]
        |]
      """.stripMargin.trim

    check(root, table)
  }

  test("List of tables") {
    val root = Root(List(Pair("points", Arr(List(
      Tbl(Map("x" -> Num(1), "y" -> Num(2), "z" -> Num(3))),
      Tbl(Map("x" -> Num(7), "y" -> Num(8), "z" -> Num(9))),
      Tbl(Map("x" -> Num(2), "y" -> Num(4), "z" -> Num(8)))
    )))))

    val table =
      """
        |points = [
        |  { x = 1, y = 2, z = 3 },
        |  { x = 7, y = 8, z = 9 },
        |  { x = 2, y = 4, z = 8 }
        |]
      """.stripMargin.trim

    check(root, table)
  }

  test("Escaping strings") {
    val root = Root(List(Pair("a", Str("b\"d"))))
    val result = """a = "b\"d""""
    check(root, result)
  }

  test("Encoding line break") {
    val root = Root(List(Pair("a", Str("a\nb\nc"))))
    val result = """a = "a\nb\nc""""
    check(root, result)
  }

  test("Encoding special characters") {
    val root = Root(List(Pair("a", Str("\b\t\n\f\r\"\\"))))
    val result = """a = "\b\t\n\f\r\"\\""""
    val generated = Generate.generate(root)
    check(root, result)
  }

  test("Encoding positive number") {
    val root = Root(List(Pair("a", Num(100))))
    val result = """a = 100"""
    check(root, result)
  }

  test("Encoding negative number") {
    val root = Root(List(Pair("a", Num(-100))))
    val result = """a = -100"""
    check(root, result)
  }

  test("Encoding positive real value") {
    // See https://github.com/scala-native/scala-native/pull/1298
    if (System.getProperty("java.vm.name") != "Scala Native") {
      val root = Root(List(Pair("a", Real(100.12))))
      val result = """a = 100.12"""
      check(root, result)
    }
  }

  test("Encoding negative real value") {
    if (System.getProperty("java.vm.name") != "Scala Native") {
      val root = Root(List(Pair("a", Real(-100.12))))
      val result = """a = -100.12"""
      check(root, result)
    }
  }
}
