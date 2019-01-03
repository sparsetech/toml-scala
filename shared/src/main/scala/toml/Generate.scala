package toml

object Generate {
  def generate(value: Value, level: Int = 0): String = value match {
    case Value.Str(v) => "\"" + Escape.escapeJavaString(v) + "\""
    case Value.Bool(v) => if (v) "true" else "false"
    case Value.Real(v) => v.toString
    case Value.Num(v) => v.toString
    case Value.Tbl(vs) =>
      "{ " +
      vs.map { case (k, v) => k + " = " + generate(v) }.mkString(", ") +
      " }"
    case Value.Arr(vs) =>
      val (arrStart, separator, arrEnd) =
        if (vs.length > 1 && level == 0)
          ("\n  ", ",\n  ", "\n")
        else ("", ", ", "")

      "[" +
        vs.map(generate(_, level + 1)).mkString(arrStart, separator, arrEnd) +
      "]"
  }

  private def generateRef(ref: List[String]): String =
    ref.map(part =>
      if (part.exists(c => c == '.' ||
        Constants.EscapeChars.contains(c) ||
        Constants.WhitespaceChars.contains(c))
      ) "\"" + Escape.escapeJavaString(part) + "\"" else part
    ).mkString(".")

  def generate(node: Node): String = node match {
    case Node.Pair(k, v) => k + " = " + generate(v)
    case Node.NamedTable(ref, values) =>
      "[" + generateRef(ref) + "]\n" +
      values.mapValues(generate(_)).map { case (k, v) =>
        k + " = " + v
      }.mkString("\n")
    case Node.NamedArray(ref, values) =>
      "[[" + generateRef(ref) + "]]\n" +
       values.mapValues(generate(_)).map { case (k, v) =>
         k + " = " + v
       }.mkString("\n")
  }

  def generate(root: Root): String =
    root.nodes.zip(root.nodes.tail.map(Some(_)) :+ None).map {
      case (node: Node.Pair, Some(_: Node.Pair)) => generate(node)
      case (node, Some(_)) => generate(node) + "\n"
      case (node, None) => generate(node)
    }.mkString("\n")
}
