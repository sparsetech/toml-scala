package toml

import java.time.{LocalDateTime, LocalTime, LocalDate, OffsetDateTime => JOffsetDateTime}

trait PlatformValue {
  case class Date          (value : LocalDate      ) extends Value
  case class Time          (value : LocalTime      ) extends Value
  case class DateTime      (value : LocalDateTime  ) extends Value
  case class OffsetDateTime(value : JOffsetDateTime) extends Value
}