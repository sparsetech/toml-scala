package toml

import fastparse.all._

private[toml] case class NamedFunction[T, V](f: T => V, name: String)
  extends (T => V)
{
  def apply(t: T) = f(t)
  override def toString: String = name
}

object Rules extends PlatformRules {
  import Constants._

  val UntilNewline = NamedFunction(!CrLf.contains(_: Char), "UntilNewline")

  val newLine    = P(StringIn(CrLf, Lf))
  val charsChunk = P(CharsWhile(UntilNewline))
  val comment    = P("#" ~ charsChunk.? ~ &(newLine | End))

  val whitespace  = P(CharIn(WhitespaceChars))
  val whitespaces = P(whitespace.rep(1))

  val skip = P(NoCut(NoTrace((whitespaces | comment | newLine).rep)))

  val letter  = P(CharIn(LettersRange))
  val letters = P(letter.rep(1))
  val digit   = P(CharIn(NumbersRange))
  val digits  = P(digit.rep(1))

  val skipSpaces = P(CharsWhile(_.isWhitespace).?)

  val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")
  val strChars    = P(CharsWhile(StringChars))

  val hexDigit       = P(CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))
  val unicodeEsc     = P("u" ~ hexDigit.rep(4))
  val unicodeEscLong = P("U" ~ hexDigit.rep(8))
  val escape         = P("\\" ~ (
    CharIn("\"/\\bfnrt") | unicodeEsc | unicodeEscLong
  ))

  val basicStr: Parser[Value.Str] =
    P("\"" ~/ (strChars | escape).rep.! ~ "\"")
      .map(str => Value.Str(Unescape.unescapeJavaString(str)))
  val literalStr: Parser[Value.Str] =
    P(
      SingleQuote.toString ~/
      (!SingleQuote.toString ~ AnyChar).rep.! ~
      SingleQuote.toString
    ).map(Value.Str)
  val multiLineBasicStr: Parser[Value.Str] =
    P(
      MultiLineDoubleQuote ~/
      skipSpaces ~
      (!MultiLineDoubleQuote ~ AnyChar).rep.! ~
      MultiLineDoubleQuote
    ).map(str => Value.Str(Unescape.unescapeJavaString(str)))
  val multiLineLiteralStr: Parser[Value.Str] =
    P(
      MultiLineSingleQuote ~/
      skipSpaces ~
      (!MultiLineSingleQuote ~ AnyChar).rep.! ~
      MultiLineSingleQuote
    ).map(Value.Str)

  val string: Parser[Value.Str] = P(
    multiLineBasicStr   |
    multiLineLiteralStr |
    basicStr            |
    literalStr)

  def rmUnderscore(s: String) = s.replace("_", "")

  val +- = P(CharIn(List('+', '-')))
  val integral = P(digits.rep(min = 1, sep = "_"))
  val fractional = P("." ~ integral)
  val exponent = P(CharIn("eE") ~ +-.? ~ integral)
  val integer: Parser[Value.Num] =
    P(+-.? ~ integral).!.map(s => Value.Num(rmUnderscore(s).toLong))
  val double: Parser[Value.Real] =
    P(+-.? ~ integral ~ (fractional | exponent)).!.map(s =>
      Value.Real(rmUnderscore(s).toDouble))

  val `true`  = P("true") .map(_ => Value.Bool(true))
  val `false` = P("false").map(_ => Value.Bool(false))
  val boolean = P(`true` | `false`)

  val dashes = P(CharIn(Dashes))
  val bareKey = P((letters | digits | dashes).rep(min = 1)).!
  val validKey: Parser[String] =
    P(bareKey | NoCut(basicStr) | NoCut(literalStr)).!
  lazy val pair: Parser[(String, Value)] =
    P(validKey ~ whitespaces.? ~ "=" ~ whitespaces.? ~ elem)
  lazy val array: Parser[Value.Arr] =
    P("[" ~ skip ~ elem.rep(sep = "," ~ skip) ~ ",".? ~ skip ~ "]")
      .map(l => Value.Arr(l.toList))

  val tableIds: Parser[Seq[String]] =
    P(validKey.rep(min = 1, sep = whitespaces.? ~ "." ~ whitespaces.?))
  val tableDef: Parser[Seq[String]] =
    P("[" ~ whitespaces.? ~ tableIds ~ whitespaces.? ~ "]")
  val tableArrayDef: Parser[Seq[String]] =
    P("[[" ~ whitespaces.? ~ tableIds ~ whitespaces.? ~ "]]")

  val pairNode: Parser[Node.Pair] = pair.map { case (k, v) => Node.Pair(k, v) }
  val table: Parser[Node.NamedTable] =
    P(skip ~ tableDef ~ skip ~ pair.rep(sep = skip)).map { case (a, b) =>
      Node.NamedTable(
        a.map(ParserUtil.cleanStr).toList,
        b.toMap)
    }
  val tableArray: Parser[Node.NamedArray] =
    P(skip ~ tableArrayDef ~ skip ~ pair.rep(sep = skip)).map { case (a, b) =>
      Node.NamedArray(
        a.map(ParserUtil.cleanStr).toList,
        b.toMap)
    }

  val inlineTable: Parser[Value.Tbl] =
    P("{" ~ skip ~ pair.rep(sep = "," ~ skip) ~ "}")
      .map(p => Value.Tbl(p.toMap))

  lazy val elem: Parser[Value] = P {
    skip ~
    (date | string | boolean | double | integer | array | inlineTable) ~
    skip
  }

  lazy val node: Parser[Node] = P(skip ~ (pairNode | table | tableArray))

  val root: Parser[Root] = P(node.rep(sep = skip) ~ skip ~ End)
    .map(nodes => Root(nodes.toList))
}