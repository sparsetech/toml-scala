package toml

import java.time.{LocalDateTime, LocalTime, LocalDate, OffsetDateTime}

trait PlatformCodecs {
  implicit val localDateCodec: Codec[LocalDate] = Codec {
    case (Value.Date(value), _, _) => Right(value)
    case (value            , _, _) =>
      Left((List.empty, s"LocalDate expected, $value provided"))
  }

  implicit val localTimeCodec: Codec[LocalTime] = Codec {
    case (Value.Time(value), _, _) => Right(value)
    case (value            , _, _) =>
      Left((List.empty, s"LocalTime expected, $value provided"))
  }

  implicit val localDateTimeCodec: Codec[LocalDateTime] = Codec {
    case (Value.DateTime(value), _, _) => Right(value)
    case (value                , _, _) =>
      Left((List.empty, s"LocalDateTime expected, $value provided"))
  }

  implicit val offsetDateTimeCodec: Codec[OffsetDateTime] = Codec {
    case (Value.OffsetDateTime(value), _, _) => Right(value)
    case (value                      , _, _) =>
      Left((List.empty, s"OffsetDateTime expected, $value provided"))
  }
}
