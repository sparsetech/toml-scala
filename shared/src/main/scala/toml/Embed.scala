package toml

object Embed {
  def addPair(stack: List[String],
              result: Value.Tbl,
              key: String,
              value: Value
             ): Either[Parse.Error, Value.Tbl] =
    if (result.values.contains(key))
      Left((stack :+ key) -> "Cannot redefine value")
    else Right(Value.Tbl(result.values + (key -> value)))

  def merge[T](values: List[T])(
    f: (Value.Tbl, T) => Either[Parse.Error, Value.Tbl]
  ): Either[Parse.Error, Value.Tbl] =
    values.foldLeft(Right(Value.Tbl(Map())): Either[Parse.Error, Value.Tbl]) {
      case (Left(m), _)          => Left(m)
      case (Right(result), node) => f(result, node)
    }

  def updateTable(value: Value.Tbl,
                  stack: List[String],
                  trace: List[String],
                  insert: List[(String, Value)]
                 ): Either[Parse.Error, Value.Tbl] =
    stack match {
      case Nil =>
        merge(insert) {
          case (result, (key, value)) => addPair(trace, result, key, value)
        }

      case stackHead :: stackTail =>
        val child =
          value.values.get(stackHead) match {
            case Some(v: Value.Tbl) => updateTable(v, stackTail, trace, insert)
            case Some(Value.Arr(init :+ last)) =>
              updateTable(
                last.asInstanceOf[Value.Tbl], stackTail, trace, insert
              ).right.map(v => Value.Arr(init :+ v))
            case None => updateTable(Value.Tbl(Map()), stackTail, trace, insert)
          }

        child.right.map(c =>
          value.copy(values = value.values + (stackHead -> c)))
    }

  def addArrayRow(value: Value,
                  stack: List[String],
                  trace: List[String],
                  insert: List[(String, Value)]
                 ): Either[Parse.Error, Value] =
    stack match {
      case Nil =>
        merge(insert) {
          case (result, (key, value)) => addPair(trace, result, key, value)
        }
        .right.map(tbl =>
          value match {
            case v: Value.Arr => v.copy(values = v.values :+ tbl)
            case _: Value.Tbl => Value.Arr(List(tbl))
          })

      case stackHead :: stackTail =>
        value match {
          case Value.Arr(init :+ last) =>
            addArrayRow(last, stack, trace, insert)
              .right
              .map(v => Value.Arr(init :+ v))

          case v: Value.Tbl =>
            val oldChild = v.values.getOrElse(stackHead, Value.Tbl(Map()))
            addArrayRow(oldChild, stackTail, trace, insert).right.map(
              newChild => v.copy(values = v.values + (stackHead -> newChild)))
        }
    }

  /** Eliminates all [[Node]] instances, converting them to tables and arrays */
  def root(root: Root): Either[Parse.Error, Value.Tbl] =
    merge(root.nodes) { case (result, node) =>
      node match {
        case Node.Pair(key, value) =>
          addPair(List(), result, key, value)
        case Node.NamedArray(path, pairs) =>
          addArrayRow(result, path, path, pairs)
            .right
            .map(_.asInstanceOf[Value.Tbl])
        case Node.NamedTable(path, pairs) =>
          updateTable(result, path, path, pairs)
      }
    }
}
