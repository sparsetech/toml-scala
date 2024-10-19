package toml

object Unescape {
  def decodeUnicodeShort(str: String): String = {
    val code = Integer.parseInt(str, 16)
    Character.toChars(code).mkString
  }

  def unescapeJavaString(str: String): String = {
    val sb = new StringBuilder(str.length())

    var i = 0
    while (i < str.length) {
      val ch = str.charAt(i)
      i += 1
      val nextChar = if (i == str.length()) None else Some(str.charAt(i))

      if (ch != '\\') sb.append(ch)
      else if (nextChar.contains('\n')) {
        while (i < str.length() && Character.isWhitespace(str.charAt(i)))
          i += 1
      } else {
        i += 1

        val s = nextChar match {
          case None       => "\\"
          case Some('b')  => "\b"
          case Some('t')  => "\t"
          case Some('n')  => "\n"
          case Some('f')  => "\f"
          case Some('r')  => "\r"
          case Some('\"') => "\""
          case Some('\\') => "\\"
          case Some('u') if i + 4 <= str.length =>  // U+XXXX
            val dec = str.slice(i, i + 4)
            i += 4
            decodeUnicodeShort(dec)
          case Some('U') if i + 8 <= str.length =>  // U+XXXXXXXX
            val dec = str.slice(i, i + 8)
            i += 8
            decodeUnicodeShort(dec)
          case Some(c) => c.toString
        }

        sb.append(s)
      }
    }

    sb.toString()
  }
}
