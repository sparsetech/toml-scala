package toml

private[toml] object ParserUtil {
  import Constants._

  def cleanStr(s: String): String =
    (s.headOption, s.lastOption) match {
      case (Some(SingleQuote), Some(SingleQuote)) |
           (Some(DoubleQuote), Some(DoubleQuote)) => s.init.tail
      case _ => s
    }
}
