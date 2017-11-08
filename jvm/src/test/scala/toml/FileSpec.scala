package toml

import java.time._
import java.io.File

import org.scalatest.{FunSuite, Matchers}
import fastparse.core.Parsed._

class FileSpec extends FunSuite with Matchers {
  import TestHelpers._

  test("Parse multi-line strings") {
    val toml =
      io.Source.fromFile(new File("shared/src/test/toml/strings.toml"))
        .mkString
    val nodes = testSuccess(toml).nodes
    assert(nodes(0) == Node.Pair("str1", Value.Str("The quick brown fox jumps over the lazy dog.")))
    assert(nodes(1) == Node.Pair("str2", Value.Str("The quick brown fox jumps over the lazy dog.")))
    assert(nodes(2) == Node.Pair("str3", Value.Str("The quick brown fox jumps over the lazy dog.")))
  }

  test("Parse the hard TOML example") {
    val toml =
      io.Source.fromFile(new File("shared/src/test/toml/hard_example.toml"))
        .mkString
    testSuccess(toml)
  }
}
