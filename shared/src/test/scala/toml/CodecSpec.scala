package toml

import org.scalatest.FunSuite

import Codecs._

class CodecSpec extends FunSuite {
  test("Booleans") {
    case class BoolPair(a: Boolean, b: Boolean)

    val pair =
      """
        |a = true
        |b = false
      """.stripMargin

    val result = Toml.parseAs[BoolPair](pair)

    assert(result == Right(BoolPair(a = true, b = false)))
  }

  test("Strings") {
    val elem = Rules.elem.parse("\"test\"").get.value
    val result = new Toml.CodecHelperValue[String].apply(elem)
    assert(result == Right("test"))
  }

  test("Strings (2)") {
    case class Pair(a: String)

    val pair = """a = ''"""
    assert(Toml.parseAs[Pair](pair) == Right(Pair("")))

    val pair2 = """a = """""
    assert(Toml.parseAs[Pair](pair2) == Right(Pair("")))

    val pair3 = """a = 'Tom "Dubs" Preston-Werner'"""
    assert(Toml.parseAs[Pair](pair3) == Right(Pair("Tom \"Dubs\" Preston-Werner")))

    val pair4 =
      """a = '''
        |I [dw]on't need \d{2} apples'''""".stripMargin
    assert(Toml.parseAs[Pair](pair4) == Right(Pair(
      """I [dw]on't need \d{2} apples""")))

    val pair5 = "a = \"\"\"\nRoses\nViolets\"\"\""
    assert(Toml.parseAs[Pair](pair5) == Right(Pair(
      "Roses\nViolets")))

    val pair6 = "a = \"Roses\\nViolets\""
    assert(Toml.parseAs[Pair](pair6) == Right(Pair(
      "Roses\nViolets")))
  }

  test("Lists") {
    val elem = Rules.elem.parse("""["test", "a"]""").get.value
    val result = new Toml.CodecHelperValue[List[String]].apply(elem)
    assert(result == Right(List("test", "a")))
  }

  test("Pairs") {
    case class Pair(a: Int)

    val pair = """a = 1"""
    assert(Toml.parseAs[Pair](pair) == Right(Pair(1)))

    case class Pairs(a: Int, b: Int)
    val pairs =
      """b = 2
        |a = 1
      """.stripMargin
    assert(Toml.parseAs[Pairs](pairs) == Right(Pairs(1, 2)))
  }

  test("Unknown pair") {
    case class Pair(a: Int)

    val pairs =
      """b = 2
        |a = 1
      """.stripMargin
    val res = Toml.parseAs[Pair](pairs)
    assert(res == Left(List("b"), "Unknown field"))
  }

  test("Table") {
    case class Table(a: Int)
    case class Root(table: Table)

    val table =
      """
        |[table]
        |a = 1
      """.stripMargin
    assert(Toml.parseAs[Root](table) == Right(Root(Table(1))))
  }

  test("Table (2)") {
    case class Table(b: Int)
    case class Root(a: Int, table: Table)

    val table =
      """
        |a = 1
        |[table]
        |b = 2
      """.stripMargin
    assert(Toml.parseAs[Root](table) == Right(Root(1, Table(2))))
  }

  test("Table (3)") {
    case class Table2(value : Int   )
    case class Table (table2: Table2)
    case class Root  (table : Table )

    val table =
      """
        |[table.table2]
        |value = 42
      """.stripMargin
    assert(Toml.parseAs[Root](table) == Right(Root(Table(Table2(42)))))
  }

  test("Table (4)") {
    case class TaterMan(`type`: String)
    case class Dog(`tater.man`: TaterMan)
    case class Root(dog: Dog)

    val table =
      """
        |[dog."tater.man"]
        |type = "pug"
      """.stripMargin
    assert(Toml.parseAs[Root](table) == Right(Root(Dog(TaterMan("pug")))))
  }

  test("Table (5)") {
    case class Table(b: Int)
    case class Root(a: Int, table: Option[Table])

    val table = "a = 1"
    assert(Toml.parseAs[Root](table) == Right(Root(1, None)))
  }

  test("Table (6)") {
    case class Table3(value : Int)
    case class Table2(value : Int)
    case class Table (table2: Table2, table3: Table3)
    case class Root  (table : Table)

    val table =
      """
        |[table.table2]
        |value = 23
        |[table.table3]
        |value = 42
      """.stripMargin
    assert(Toml.parseAs[Root](table)
       == Right(Root(Table(Table2(23), Table3(42)))))
  }

  test("Unknown table") {
    case class Table(a: Int)
    case class Root(table: Table = Table(42))

    val table =
      """
        |[table2]
        |a = 1
      """.stripMargin
    assert(Toml.parseAs[Root](table) == Left(List("table2"), "Unknown field"))
  }

  test("Inline table") {
    val table = "point = { x = 1, y = 2 }"

    case class Point(x: Int, y: Int)
    case class Root(point: Point)

    assert(Toml.parseAs[Root](table) == Right(Root(Point(1, 2))))
  }

  test("Inline list of tables") {
    val tableList =
      """
        |points = [ { x = 1, y = 2, z = 3 },
        |           { x = 7, y = 8, z = 9 },
        |           { x = 2, y = 4, z = 8 } ]
      """.stripMargin

    case class Point(x: Int, y: Int, z: Int)
    case class Root(points: List[Point])

    assert(Toml.parseAs[Root](tableList) == Right(Root(List(
      Point(1, 2, 3),
      Point(7, 8, 9),
      Point(2, 4, 8)))))
  }

  test("Inline list of tables with unknown field") {
    val tableList =
      """
        |points = [ { x = 1, y = 2 },
        |           { x = 7, y = 8, z = 9 } ]
      """.stripMargin

    case class Point(x: Int, y: Int)
    case class Root(points: List[Point])

    assert(Toml.parseAs[Root](tableList) ==
           Left(List("points", "z"), "Unknown field"))
  }

  test("Inline list of tuples") {
    val tableList =
      """
        |points = [ [ 1, "2", 3 ],
        |           [ 7, "8", 9 ],
        |           [ 2, "4", 8 ] ]
      """.stripMargin

    case class Point(x: Int, y: String, z: Int)
    case class Root(points: List[Point])

    assert(Toml.parseAs[Root](tableList) == Right(Root(List(
      Point(1, "2", 3),
      Point(7, "8", 9),
      Point(2, "4", 8)))))
  }

  test("Inline list of tuples (2)") {
    val tableList = """points = [ [ ] ]"""

    case class Point(x: Int)
    case class Root(points: List[Point])

    assert(Toml.parseAs[Root](tableList) == Left((List("points"), "Cannot resolve `x`")))
  }

  test("Inline list of tuples with default values") {
    val tableList = """points = [ [ 1, "2" ], [ 7 ], [ ] ]"""

    case class Point(x: Int = 23, y: String = "y", z: Int = 42)
    case class Root(points: List[Point])

    assert(Toml.parseAs[Root](tableList) == Right(Root(List(
      Point(1, "2", 42),
      Point(7, "y", 42),
      Point(23, "y", 42)))))
  }

  test("Inline list of tuples with default values (2)") {
    val tableList = """points = [ [ 1, "2" ], [ 7 ], [ ] ]"""

    case class Point(x: Int = 23, y: Option[String])
    case class Root(points: List[Point])

    assert(Toml.parseAs[Root](tableList) == Right(Root(List(
      Point(1, Some("2")),
      Point(7, None),
      Point(23, None)))))
  }

  test("Inline list of tuples with default values (3)") {
    val tableList = """points = [ [ 1, "2" ], [ 7 ], [ ] ]"""

    case class Point(x: Option[Int] = Some(23), y: Option[String])
    case class Root(points: List[Point])

    assert(Toml.parseAs[Root](tableList) == Right(Root(List(
      Point(Some(1), Some("2")),
      Point(Some(7), None),
      Point(Some(23), None)))))
  }

  test("Inline list of tuples with default values (4)") {
    val tableList = """points = [ [ 1, "2" ] ]"""

    case class Point(x: Option[Int] = Some(23))
    case class Root(points: List[Point])

    assert(Toml.parseAs[Root](tableList) == Left(List("points"),
      "Too many elements; remove Str(2)"))
  }

  test("Array of tables (1)") {
    case class Product(name  : String,
                       sku   : Int,
                       colour: String)
    case class Root(products: List[Product])

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

    assert(Toml.parseAs[Root](array) == Right(Root(List(
      Product("Hammer", 738594937, "blue"),
      Product("Nail", 284758393, "grey")))))
  }

  test("Array of tables (2)") {
    case class Product(name  : Option[String] = Option.empty,
                       sku   : Option[Int]    = Option.empty,
                       colour: Option[String] = Option.empty)
    case class Root(products: List[Product])

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

    assert(Toml.parseAs[Root](array) == Right(Root(List(
      Product(Some("Hammer"), Some(738594937), None),
      Product(None, None, None),
      Product(Some("Nail"), Some(284758393), Some("grey"))))))
  }

  test("Array of tables (3)") {
    case class Physical(colour: String, shape: String)
    case class Variety(name: String)
    case class Fruit(name: String,
                     physical: Option[Physical],
                     variety: List[Variety])
    case class Root(fruit: List[Fruit])

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

    assert(Toml.parseAs[Root](array) == Right(Root(List(
      Fruit("apple", Some(Physical("red", "round")), List(
        Variety("red delicious"),
        Variety("granny smith")
      )),
      Fruit("banana", None, List(Variety("plantain")))))))
  }

  test("Table codec") {
    val array =
      """
        |a = 23
        |b = 42
      """.stripMargin

    assert(Toml.parseAsValue[Map[String, Int]](array) == Right(Map(
      "a" -> 23,
      "b" -> 42
    )))
  }

  test("Error handling") {
    case class Root(a: String)

    val toml = "a = 1"
    assert(Toml.parseAs[Root](toml) ==
      Left((List("a"), "String expected, Num(1) provided")))
  }

  test("Error handling (2)") {
    case class Root(a: String)

    val toml = ""
    assert(Toml.parseAs[Root](toml) ==
      Left((List.empty, "Cannot resolve `a`")))
  }

  test("Error handling (3)") {
    case class A(b: Int)
    case class Root(a: A)

    val toml = "a = 1"
    assert(Toml.parseAs[Root](toml) ==
      Left(List("a"), "Cannot resolve `b`"))
  }

  test("Error handling (4)") {
    case class B(c: Int)
    case class A(b: B)
    case class Root(a: A)

    val toml = "a = { b = 42 }"
    assert(Toml.parseAs[Root](toml) ==
      Left(List("a", "b"), "Cannot resolve `c`"))
  }

  test("Error handling (5)") {
    case class A(value: Int)
    case class B(value: Int)
    case class Root(a: A, b: B)

    val toml =
      """
        |a = { value = 42 }
        |b = { }
      """.stripMargin

    assert(Toml.parseAs[Root](toml) ==
      Left(List("b"), "Cannot resolve `value`"))
  }

  test("Error handling (6)") {
    // Despite of the default value, an error must be triggered
    case class Module(a: Option[Module] = None,
                      b: List[List[Int]] = List())
    case class Root(module: Map[String, Module])
    val toml =
      """
        |[module.name.a]
        |b = [1, 2, 3]
      """.stripMargin
    assert(Toml.parseAs[Root](toml) ==
           Left(List("module", "name", "a", "b"), "List expected, Num(1) provided"))
  }

  test("Default parameters (1)") {
    case class Root(a: Int, b: Int = 42)
    assert(Toml.parseAs[Root]("a = 23") == Right(Root(23, 42)))
  }

  test("Default parameters (2)") {
    case class A(value: Int, value2: Int = 99)
    case class B(value: Int, value2: Int = 100)
    case class Root(a: A, b: B)

    val toml =
      """
        |a = { value = 42, value2 = 88 }
        |b = { value = 23 }
      """.stripMargin

    assert(Toml.parseAs[Root](toml) == Right(Root(A(42, 88), B(23, 100))))
  }

  test("Default parameters (3)") {
    case class A(value: Int)
    case class B(value: Int)
    case class Root(a: A, b: B = B(42))

    val toml = "a = { value = 23 }"
    assert(Toml.parseAs[Root](toml) == Right(Root(A(23), B(42))))
  }

  test("Default parameters (4)") {
    case class A(v1: Int)
    case class B(v2: Int = 42)
    case class Root(a: A, b: Option[B])

    val toml =
      """
        |a = { v1 = 23 }
        |b = {}
      """.stripMargin
    assert(Toml.parseAs[Root](toml) == Right(Root(A(23), Some(B(42)))))
  }

  test("Default parameters (5)") {
    case class A(value1: Option[String], value2: Int = 42)
    case class Root(a: A)

    val toml = "a = { }"
    assert(Toml.parseAs[Root](toml) == Right(Root(A(None, 42))))
  }
}
