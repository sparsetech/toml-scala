package toml

import org.scalatest.FunSuite

import Value._

class EmbedSpec extends FunSuite {
  test("One pair") {
    val pair = """a = 1"""
    val node = Rules.root.parse(pair).get.value
    assert(Embed.root(node) == Right(Tbl(Map("a" -> Num(1)))))
  }

  test("Two pairs") {
    val pairs =
      """b = 2
        |a = 1
      """.stripMargin
    val node2 = Rules.root.parse(pairs).get.value
    assert(Embed.root(node2) == Right(Tbl(Map(
      "a" -> Num(1), "b" -> Num(2)))))
  }

  test("Simple table") {
    val table =
      """
        |[table]
        |a = 1
      """.stripMargin
    val node = Rules.root.parse(table).get.value
    assert(Embed.root(node) ==
      Right(Tbl(Map("table" -> Tbl(Map("a" -> Num(1)))))))
  }

  test("Pair and table") {
    val table =
      """
        |a = 1
        |[table]
        |b = 2
      """.stripMargin
    val node = Rules.root.parse(table).get.value
    assert(Embed.root(node) ==
      Right(Tbl(Map("a" -> Num(1), "table" -> Tbl(Map("b" -> Num(2)))))))
  }

  test("Nested table") {
    val table =
      """
        |[table.table2]
        |value = 42
      """.stripMargin
    val node = Rules.root.parse(table).get.value
    assert(Embed.root(node) ==
      Right(Tbl(Map("table" ->
        Tbl(Map("table2" ->
          Tbl(Map("value" -> Num(42)))))))))
  }

  test("Two nested tables") {
    val table =
      """
        |[table.table2]
        |value = 23
        |[table.table3]
        |value = 42
      """.stripMargin
    val node = Rules.root.parse(table).get.value
    assert(Embed.root(node) ==
      Right(Tbl(Map(
        "table" -> Tbl(Map(
          "table2" -> Tbl(Map("value" -> Num(23))),
          "table3" -> Tbl(Map("value" -> Num(42)))))))))
  }

  test("Empty nested tables") {
    val table =
      """
        |[table.table2]
        |[table.table3]
        |value = 42
      """.stripMargin
    val node = Rules.root.parse(table).get.value
    assert(Embed.root(node) ==
      Right(Tbl(Map(
        "table" -> Tbl(Map(
          "table2" -> Tbl(Map.empty),
          "table3" -> Tbl(Map("value" -> Num(42)))))))))
  }


  test("Inline table list") {
    val tableList =
      """
        |points = [ { x = 1, y = 2, z = 3 },
        |           { x = 7, y = 8, z = 9 },
        |           { x = 2, y = 4, z = 8 } ]
      """.stripMargin
    val node = Rules.root.parse(tableList).get.value
    assert(Embed.root(node) ==
      Right(Tbl(Map(
        "points" -> Arr(List(
          Tbl(Map("x" -> Num(1), "y" -> Num(2), "z" -> Num(3))),
          Tbl(Map("x" -> Num(7), "y" -> Num(8), "z" -> Num(9))),
          Tbl(Map("x" -> Num(2), "y" -> Num(4), "z" -> Num(8)))))))))
  }

  test("Array") {
    val array =
      """
        |[[products]]
        |name = "Hammer"
        |sku = 738594937
        |colour = "blue"
        |
        |[[products]]
        |name = "Nail"
        |sku = 284758393
        |colour = "grey"
      """.stripMargin

    val node = Rules.root.parse(array).get.value
    assert(Embed.root(node) == Right(Tbl(Map(
      "products" -> Arr(List(
        Tbl(Map("name" -> Str("Hammer"), "sku" -> Num(738594937), "colour" -> Str("blue"))),
        Tbl(Map("name" -> Str("Nail")  , "sku" -> Num(284758393), "colour" -> Str("grey")))
      ))))))
  }

  test("Array with empty items") {
    val array =
      """
        |[[products]]
        |name = "Hammer"
        |sku = 738594937
        |
        |[[products]]
        |
        |[[products]]
        |name = "Nail"
        |sku = 284758393
        |colour = "grey"
      """.stripMargin

    val node = Rules.root.parse(array).get.value
    assert(Embed.root(node) == Right(Tbl(Map(
      "products" -> Arr(List(
        Tbl(Map("name" -> Str("Hammer"), "sku" -> Num(738594937))),
        Tbl(Map.empty),
        Tbl(Map("name" -> Str("Nail"), "sku" -> Num(284758393), "colour" -> Str("grey")))
      ))))))
  }

  test("Nested array") {
    val array =
      """
        |[[fruit]]
        |  name = "apple"
        |
        |  [fruit.physical]
        |    colour = "red"
        |    shape  = "round"
        |
        |  [[fruit.variety]]
        |    name = "red delicious"
        |
        |  [[fruit.variety]]
        |    name = "granny smith"
        |
        |[[fruit]]
        |  name = "banana"
        |
        |  [[fruit.variety]]
        |    name = "plantain"
      """.stripMargin

    val node = Rules.root.parse(array).get.value
    assert(Embed.root(node) == Right(Tbl(Map(
      "fruit" -> Arr(List(
        Tbl(Map(
          "name" -> Str("apple"),
          "physical" -> Tbl(Map(
            "colour" -> Str("red"),
            "shape"  -> Str("round")
          )),
          "variety" -> Arr(List(
            Tbl(Map("name" -> Str("red delicious"))),
            Tbl(Map("name" -> Str("granny smith")))
          ))
        )),
        Tbl(Map(
          "name" -> Str("banana"),
          "variety" -> Arr(List(
            Tbl(Map("name" -> Str("plantain")))
          ))
        ))
      ))
    ))))
  }
}
