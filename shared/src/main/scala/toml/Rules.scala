package toml

import fastparse._
import NoWhitespace._

private[toml] case class NamedFunction[T, V](f: T => V, name: String)
  extends (T => V)
{
  def apply(t: T) = f(t)
  override def toString: String = name
}

sealed trait Extension
object Extension {
  case object MultiLineInlineTables extends Extension
}

case object Rules extends toml.Rules(Set())

class Rules(extensions: Set[Extension]) extends PlatformRules {
  import Constants._
  import Extension._

  val UntilNewline = NamedFunction(!CrLf.contains(_: Char), "UntilNewline")

  def newLine[$: P]    = P(StringIn(CrLf, Lf))
  def charsChunk[$: P] = P(CharsWhile(UntilNewline))
  def comment[$: P]    = P("#" ~ charsChunk.? ~ &(newLine | End))
  def whitespace[$: P] = P(CharIn(WhitespaceChars))

  def skip[$: P]   = P(NoCut(NoTrace((whitespace | comment | newLine).rep)))
  def skipWs[$: P] = P(NoCut(NoTrace(whitespace.rep)))

  def letter[$: P] = P(CharIn(LettersRange))
  def digit[$: P]  = P(CharIn(NumbersRange))
  def digits[$: P] = P(digit.rep(1))
  def dash[$: P]   = P(CharIn(Dashes))

  val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")
  def strChars[$: P] = P(CharsWhile(StringChars))


  def hexDigit[$: P]       = P(CharIn("0-9", "a-f", "A-F"))
  def unicodeEsc[$: P]     = P("u" ~ hexDigit.rep(4))
  def unicodeEscLong[$: P] = P("U" ~ hexDigit.rep(8))
  def escape[$: P]         = P("\\" ~ (
    CharIn("\"/\\bfnrt") | unicodeEsc | unicodeEscLong
  ))

  def basicStr[$: P]: P[Value.Str] =
    P(DoubleQuote.toString ~/ (escape | strChars).rep.! ~ DoubleQuote.toString)
      .map(str => Value.Str(Unescape.unescapeJavaString(str)))
  def literalStr[$: P]: P[Value.Str] =
    P(
      SingleQuote.toString ~/
      (!SingleQuote.toString ~ AnyChar).rep.! ~
      SingleQuote.toString
    ).map(Value.Str.apply)
  def multiLineBasicStr[$: P]: P[Value.Str] =
    P(
      MultiLineDoubleQuote ~/
      newLine.? ~
      (!MultiLineDoubleQuote ~ AnyChar).rep.! ~
      MultiLineDoubleQuote
    ).map(str => Value.Str(Unescape.unescapeJavaString(str)))
  def multiLineLiteralStr[$: P]: P[Value.Str] =
    P(
      MultiLineSingleQuote ~/
      newLine.? ~
      (!MultiLineSingleQuote ~ AnyChar).rep.! ~
      MultiLineSingleQuote
    ).map(Value.Str.apply)

  def string[$: P]: P[Value.Str] = P(
    multiLineBasicStr   |
    multiLineLiteralStr |
    basicStr            |
    literalStr)

  def rmUnderscore(s: String) = s.replace("_", "")

  def sign[$: P] = P(CharIn("+\\-"))
  def integral[$: P] = P(digits.rep(min = 1, sep = "_"))
  def fractional[$: P] = P("." ~ integral)
  def exponent[$: P] = P(CharIn("eE") ~ sign.? ~ integral)
  def integer[$: P]: P[Value.Num] =
    P(sign.? ~ integral).!.map(s => Value.Num(rmUnderscore(s).toLong))
  def double[$: P]: P[Value.Real] =
    P(
      sign.?.! ~
      (
        P("inf").map(_ => Double.PositiveInfinity) |
        P("nan").map(_ => Double.NaN)              |
        P(integral ~ (
          (fractional ~ exponent) |
          fractional              |
          exponent
        )).!.map(s => rmUnderscore(s).toDouble)
      )
    ).map { case (sign, value) =>
      if (sign == "-") Value.Real(-value) else Value.Real(value)
    }

  def `true`[$: P]  = P("true") .map(_ => Value.Bool(true))
  def `false`[$: P] = P("false").map(_ => Value.Bool(false))
  def boolean[$: P] = P(`true` | `false`)

  def bareKey[$: P] = P((letter | digit | dash).rep(1)).!
  def validKey[$: P]: P[String] =
    P(NoCut(basicStr.map(_.value)) | NoCut(literalStr.map(_.value)) | bareKey)
  def pair[$: P]: P[(String, Value)] =
    P(validKey ~ skipWs ~ "=" ~ skipWs ~ elem)
  def array[$: P]: P[Value.Arr] =
    P("[" ~ skip ~ elem.rep(sep = skip ~ "," ~ skip) ~ ",".? ~ skip ~ "]")
      .map(l => Value.Arr(l.toList))
  def inlineTable[$: P]: P[Value.Tbl] =
    (if (extensions.contains(MultiLineInlineTables))
      P("{" ~ skip ~ pair.rep(sep = skip ~ "," ~ skip) ~ ",".? ~ skip ~ "}")
     else
      P("{" ~ skipWs ~ pair.rep(sep = skipWs ~ "," ~ skipWs) ~ skipWs ~ "}")
    ).map(p => Value.Tbl(p.toMap))

  def tableIds[$: P]: P[Seq[String]] =
    P(validKey.rep(min = 1, sep = skipWs ~ "." ~ skipWs).map(_.toSeq))
  def tableDef[$: P]: P[Seq[String]] =
    P("[" ~ skipWs ~ tableIds ~ skipWs ~ "]")
  def tableArrayDef[$: P]: P[Seq[String]] =
    P("[[" ~ skipWs ~ tableIds ~ skipWs ~ "]]")

  def pairNode[$: P]: P[Node.Pair] = pair.map { case (k, v) => Node.Pair(k, v) }
  def table[$: P]: P[Node.NamedTable] =
    P(tableDef ~ skip ~ pair.rep(sep = skip)).map { case (a, b) =>
      Node.NamedTable(a.toList, b.toList)
    }
  def tableArray[$: P]: P[Node.NamedArray] =
    P(tableArrayDef ~ skip ~ pair.rep(sep = skip)).map { case (a, b) =>
      Node.NamedArray(a.toList, b.toList)
    }

  def elem[$: P]: P[Value] =
    P(date | string | boolean | double | integer | array | inlineTable)

  def node[$: P]: P[Node] = P(pairNode | table | tableArray)
  def root[$: P]: P[Root] = P(skip ~ node.rep(sep = skip) ~ skip ~ End)
    .map(nodes => Root(nodes.toList))
}
