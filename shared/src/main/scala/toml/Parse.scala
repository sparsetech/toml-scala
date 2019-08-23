package toml

object Parse {
  type Field    = String
  type Address  = List[Field]
  type Message  = String
  type Error    = (Address, Message)
}
