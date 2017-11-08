package toml

/** Top-level nodes */
sealed trait Node

object Node {
  case class Pair(key: String, value: Value) extends Node

  case class NamedTable(ref: List[String], values: Map[String, Value])
    extends Node

  /** Reference to an array item */
  case class NamedArray(ref: List[String], values: Map[String, Value])
    extends Node
}

case class Root(nodes: List[Node])
