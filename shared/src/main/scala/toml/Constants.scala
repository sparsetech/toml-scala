package toml

object Constants {
  val SingleQuote = '\''
  val DoubleQuote = '"'

  val MultiLineSingleQuote = "'''"
  val MultiLineDoubleQuote = "\"\"\""

  val NumbersRange = '0' to '9'
  val LettersRange = ('a' to 'z') ++ ('A' to 'Z')

  val Dashes = Set('-', '_')

  val Lf   = "\n"
  val CrLf = "\r\n"

  val WhitespaceChars = Set(' ', '\t')
  val EscapeChars  = Set('\b', '\t', '\n', '\f', '\r', '"', '\\')
}
