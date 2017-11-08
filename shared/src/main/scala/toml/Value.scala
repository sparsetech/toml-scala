package toml

/*sealed*/ trait Value

object Value extends PlatformValue {
  case class Str (value : String            ) extends Value
  case class Bool(value : Boolean           ) extends Value
  case class Real(value : Double            ) extends Value
  case class Num (value : Long              ) extends Value
  case class Tbl (values: Map[String, Value]) extends Value
  case class Arr (values: List[Value]       ) extends Value
}
