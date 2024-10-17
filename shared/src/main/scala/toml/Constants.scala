package toml

object Constants {
  final val SingleQuote = '\''
  final val DoubleQuote = '"'

  final val MultiLineSingleQuote = "'''"
  final val MultiLineDoubleQuote = "\"\"\""

  final val NumbersRange = "0-9"
  final val LettersRange = "a-zA-Z"

  final val Dashes = "\\-_"

  final val Lf   = "\n"
  final val CrLf = "\r\n"

  final val WhitespaceChars = " \t"
  final val EscapeChars  = "\b\t\n\f\r\"\\"
}
