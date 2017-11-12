package toml

import java.time._

import org.scalatest.{FunSuite, Matchers}
import fastparse.core.Parsed._

class DateSpec extends FunSuite with Matchers {
  import TestHelpers._

  test("Parse local date") {
    val toml = "ld = 1979-05-27"
    val nodes = testSuccess(toml).nodes
    assert(nodes(0) == Node.Pair("ld", Value.Date(LocalDate.of(1979, 5, 27))))
  }

  test("Parse local time") {
    val toml =
      """
        |lt1 = 07:32:00
        |lt2 = 00:32:00.999999
        |lt3 = 00:32:00.555
      """.stripMargin

    val nodes = testSuccess(toml).nodes
    assert(nodes(0) == Node.Pair("lt1", Value.Time(LocalTime.of(7, 32, 0, 0))))
    assert(nodes(1) == Node.Pair("lt2", Value.Time(LocalTime.of(0, 32, 0, 999999000))))
    assert(nodes(2) == Node.Pair("lt3", Value.Time(LocalTime.of(0, 32, 0, 555000000))))
  }

  test("Parse local date time") {
    val toml =
      """
        |ldt1 = 1979-05-27T07:32:00
        |ldt2 = 1979-05-27T00:32:00.999999
      """.stripMargin
    val nodes = testSuccess(toml).nodes
    assert(nodes(0) == Node.Pair("ldt1", Value.DateTime(LocalDateTime.of(
      LocalDate.of(1979, 5, 27), LocalTime.of(7, 32, 0, 0)))))
    assert(nodes(1) == Node.Pair("ldt2", Value.DateTime(LocalDateTime.of(
      LocalDate.of(1979, 5, 27), LocalTime.of(0, 32, 0, 999999000)))))
  }

  test("Parse offset date time") {
    val toml =
      """
        |odt1 = 1979-05-27T07:32:00Z
        |odt2 = 1979-05-27T00:32:00-07:00
        |odt3 = 1979-05-27T00:32:00.999999-07:00
      """.stripMargin
    val nodes = testSuccess(toml).nodes
    assert(nodes(0) == Node.Pair("odt1", Value.OffsetDateTime(
      OffsetDateTime.of(
        LocalDateTime.of(
          LocalDate.of(1979, 5, 27), LocalTime.of(7, 32, 0)
        ), ZoneOffset.of("Z")))))
    assert(nodes(1) == Node.Pair("odt2", Value.OffsetDateTime(
      OffsetDateTime.of(
        LocalDateTime.of(
          LocalDate.of(1979, 5, 27), LocalTime.of(0, 32, 0)
        ), ZoneOffset.of("-07:00")))))
    assert(nodes(2) == Node.Pair("odt3", Value.OffsetDateTime(
      OffsetDateTime.of(
        LocalDateTime.of(
          LocalDate.of(1979, 5, 27), LocalTime.of(0, 32, 0, 999999000)
        ), ZoneOffset.of("-07:00")))))
  }

  test("Codec derivation") {
    import Codecs._

    case class Root(ld: LocalDate)

    val toml = "ld = 1979-05-27"
    assert(Toml.parseAs[Root](toml) == Right(Root(LocalDate.of(1979, 5, 27))))
  }
}
