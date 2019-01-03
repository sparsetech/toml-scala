package toml

object Escape {
  def escapeJavaString(str: String): String = {
    val sb = new StringBuilder(str.length())

    var i = 0
    while (i < str.length) {
      val ch = str.charAt(i)
      i += 1

      ch match {
        case '\b' => sb.append("\\b")
        case '\t' => sb.append("\\t")
        case '\n' => sb.append("\\n")
        case '\f' => sb.append("\\f")
        case '\r' => sb.append("\\r")
        case '"' => sb.append("\\\"")
        case '\\' => sb.append("\\\\")
        case c => sb.append(c)
      }
    }

    sb.toString()
  }
}
