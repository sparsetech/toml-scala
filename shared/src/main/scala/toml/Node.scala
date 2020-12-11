package toml

/** Top-level nodes */
sealed trait Node

object Node {
  case class Pair(key: String, value: Value) extends Node

  case class Variable(name: String, value: Value) extends Node

  case class NamedTable(ref: List[String], values: List[(String, Value)])
    extends Node

  /** Reference to an array item */
  case class NamedArray(ref: List[String], values: List[(String, Value)])
    extends Node
}

case class Root(nodes: List[Node])
