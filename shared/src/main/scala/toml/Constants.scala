package toml

object Constants {
  val SingleQuote = '\''
  val DoubleQuote = '"'

  val MultiLineSingleQuote = "'''"
  val MultiLineDoubleQuote = "\"\"\""

  val NumbersRange = '0' to '9'
  val LettersRange = ('a' to 'z') ++ ('A' to 'Z')

  val Dashes = List('-', '_')

  val Lf   = "\n"
  val CrLf = "\r\n"

  val WhitespaceChars = List(' ', '\t')
}
