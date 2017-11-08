package toml

object Embed {
  def updateTable(value: Value.Tbl,
                  stack: List[String],
                  insert: Map[String, Value]): Value.Tbl =
    stack match {
      case Nil => Value.Tbl(insert)
      case stackHead :: stackTail =>
        val child =
          value.values.get(stackHead) match {
            case Some(v: Value.Tbl) => updateTable(v, stackTail, insert)
            case Some(Value.Arr(init :+ last)) =>
              Value.Arr(init :+ updateTable(
                last.asInstanceOf[Value.Tbl], stackTail, insert))
            case None => updateTable(Value.Tbl(Map.empty), stackTail, insert)
          }

        value.copy(values = value.values + (stackHead -> child))
    }

  def addArrayRow(value: Value,
                  stack: List[String],
                  insert: Map[String, Value]): Value =
    stack match {
      case Nil =>
        value match {
          case v: Value.Arr => v.copy(values = v.values :+ Value.Tbl(insert))
          case _: Value.Tbl => Value.Arr(List(Value.Tbl(insert)))
        }

      case stackHead :: stackTail =>
        value match {
          case Value.Arr(init :+ last) =>
            Value.Arr(init :+ addArrayRow(last, stack, insert))

          case v: Value.Tbl =>
            val oldChild = v.values.getOrElse(stackHead, Value.Tbl(Map.empty))
            val newChild = addArrayRow(oldChild, stackTail, insert)

            v.copy(values = v.values + (stackHead -> newChild))
        }
    }

  /** Eliminates all [[Node]] instances, converting them to tables and arrays */
  def root(root: Root): Value.Tbl =
    root.nodes.foldLeft(Value.Tbl(Map.empty)) { (result, node) =>
      node match {
        case Node.Pair(key, value) =>
          result.copy(values = result.values + (key -> value))
        case Node.NamedArray(path, pairs) =>
          addArrayRow(result, path, pairs).asInstanceOf[Value.Tbl]
        case Node.NamedTable(path, pairs) => updateTable(result, path, pairs)
      }
    }
}
