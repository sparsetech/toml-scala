package toml

import scala.collection.mutable

object Embed {
  def map(value: Value, f: Value => Either[Parse.Error, Value]): Either[Parse.Error, Value] =
    value match {
      case Value.Tbl(values) =>
        values.foldLeft(Right(Value.Tbl(Map())): Either[Parse.Error, Value.Tbl]) {
          case (Left(m), _)          => Left(m)
          case (Right(Value.Tbl(result)), (k, v)) => f(v) match {
            case Left(e)  => Left(e)
            case Right(v) => Right(Value.Tbl(result + (k -> v)))
          }
        }
      case Value.Arr(values) =>
        values.foldLeft(Right(Value.Arr(List())): Either[Parse.Error, Value.Arr]) {
          case (Left(m), _)          => Left(m)
          case (Right(Value.Arr(result)), v) => f(v) match {
            case Left(e)  => Left(e)
            case Right(v) => Right(Value.Arr(result :+ v))
          }
        }
      case v => Right(v)
    }

  def embedValue(
    value: Value, variables: collection.Map[String, Value]
  ): Either[Parse.Error, Value] =
    value match {
      case variable: Value.Variable =>
        variables.get(variable.identifier) match {
          case None => Left(List() -> s"Variable `${variable.identifier}` not declared")
          case Some(v) => Right(v)
        }
      case _ => map(value, embedValue(_, variables))
    }

  def addPair(stack: List[String],
              variables: collection.Map[String, Value],
              result: Value.Tbl,
              key: String,
              value: Value
             ): Either[Parse.Error, Value.Tbl] =
    if (result.values.contains(key))
      Left((stack :+ key) -> "Cannot redefine value")
    else embedValue(value, variables)
      .left.map { case (_, message) => (stack :+ key, message) }
      .right.map(v => Value.Tbl(result.values + (key -> v)))

  def merge[T](values: List[T])(
    f: (Value.Tbl, T) => Either[Parse.Error, Value.Tbl]
  ): Either[Parse.Error, Value.Tbl] =
    values.foldLeft(Right(Value.Tbl(Map())): Either[Parse.Error, Value.Tbl]) {
      case (Left(m), _)          => Left(m)
      case (Right(result), node) => f(result, node)
    }

  def updateTable(value: Value.Tbl,
                  stack: List[String],
                  variables: collection.Map[String, Value],
                  trace: List[String],
                  insert: List[(String, Value)]
                 ): Either[Parse.Error, Value.Tbl] =
    stack match {
      case Nil =>
        merge(insert) { case (result, (key, value)) =>
          addPair(trace, variables, result, key, value)
        }

      case stackHead :: stackTail =>
        val child =
          value.values.get(stackHead) match {
            case Some(v: Value.Tbl) =>
              updateTable(v, stackTail, variables, trace, insert)
            case Some(Value.Arr(init :+ last)) =>
              updateTable(
                last.asInstanceOf[Value.Tbl], stackTail, variables, trace, insert
              ).right.map(v => Value.Arr(init :+ v))
            case None =>
              updateTable(Value.Tbl(Map()), stackTail, variables, trace, insert)
          }

        child.right.map(c =>
          value.copy(values = value.values + (stackHead -> c)))
    }

  def addArrayRow(value: Value,
                  stack: List[String],
                  variables: collection.Map[String, Value],
                  trace: List[String],
                  insert: List[(String, Value)]
                 ): Either[Parse.Error, Value] =
    stack match {
      case Nil =>
        merge(insert) { case (result, (key, value)) =>
          addPair(trace, variables, result, key, value)
        }
        .right.map(tbl =>
          value match {
            case v: Value.Arr => v.copy(values = v.values :+ tbl)
            case _: Value.Tbl => Value.Arr(List(tbl))
          })

      case stackHead :: stackTail =>
        value match {
          case Value.Arr(init :+ last) =>
            addArrayRow(last, stack, variables, trace, insert)
              .right
              .map(v => Value.Arr(init :+ v))

          case v: Value.Tbl =>
            val oldChild = v.values.getOrElse(stackHead, Value.Tbl(Map()))
            addArrayRow(oldChild, stackTail, variables, trace, insert).right.map(
              newChild => v.copy(values = v.values + (stackHead -> newChild)))
        }
    }

  /** Eliminates all [[Node]] instances, converting them to tables and arrays */
  def root(root: Root): Either[Parse.Error, Value.Tbl] = {
    val variables = mutable.HashMap[String, Value]()
    merge(root.nodes) { case (result, node) =>
      node match {
        case Node.Pair(key, value) =>
          addPair(List(), variables, result, key, value)
        case Node.NamedArray(path, pairs) =>
          addArrayRow(result, path, variables, path, pairs)
            .right
            .map(_.asInstanceOf[Value.Tbl])
        case Node.NamedTable(path, pairs) =>
          updateTable(result, path, variables, path, pairs)
        case Node.Variable(name, value) =>
          if (variables.contains(name))
            Left(List() -> s"Cannot redefine variable `$name`")
          else {
            embedValue(value, variables).right.map { v =>
              variables += name -> v
              result
            }
          }
      }
    }
  }
}
