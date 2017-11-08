package toml

import java.time._

import fastparse.all._

trait PlatformRules { this: Rules.type =>
  private val TenPowers =
    List(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000)

  val localTime: Parser[Value.Time] = P(
    digit.rep(2).! ~ ":" ~ digit.rep(2).! ~ ":" ~ digit.rep(2).! ~ ("." ~ digit.rep.!).?
  ).map { case (h, m, s, ns) =>
    val nano = ns.map { str =>
      val digits = str.length
      str.toInt * TenPowers(9 - digits)
    }.getOrElse(0)

    Value.Time(LocalTime.of(h.toInt, m.toInt, s.toInt, nano))
  }

  val localDate: Parser[Value.Date] = P(
    digit.rep(4).! ~ "-" ~ digit.rep(2).! ~ "-" ~ digit.rep(2).!
  ).map { case (y, m, d) =>
    Value.Date(LocalDate.of(y.toInt, m.toInt, d.toInt))
  }

  val localDateTime: Parser[Value.DateTime] = P(
    localDate ~ "T" ~ localTime
  ).map { case (date, time) =>
    Value.DateTime(LocalDateTime.of(date.value, time.value))
  }

  val offsetDateTime: Parser[Value.OffsetDateTime] = P(
    localDateTime ~ ("Z" | (("-" | "+") ~ digit.rep(2) ~ ":" ~ digit.rep(2))).!
  ).map { case (dateTime, offset) =>
    Value.OffsetDateTime(
      OffsetDateTime.of(dateTime.value, ZoneOffset.of(offset)))
  }

  val date = P(offsetDateTime | localDateTime | localDate | localTime)
}