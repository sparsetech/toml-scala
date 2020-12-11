package toml

import scala.meta.internal.fastparse.all._

private[toml] case class NamedFunction[T, V](f: T => V, name: String)
  extends (T => V)
{
  def apply(t: T) = f(t)
  override def toString: String = name
}

sealed trait Extension
object Extension {
  case object MultiLineInlineTables extends Extension
  case object Variables             extends Extension
}

case object Rules extends toml.Rules(Set())

class Rules(extensions: Set[Extension]) extends PlatformRules {
  import Constants._
  import Extension._

  val UntilNewline = NamedFunction(!CrLf.contains(_: Char), "UntilNewline")

  val newLine    = P(StringIn(CrLf, Lf))
  val charsChunk = P(CharsWhile(UntilNewline))
  val comment    = P("#" ~ charsChunk.? ~ &(newLine | End))
  val whitespace = P(CharIn(WhitespaceChars.toList))

  val skip   = P(NoCut(NoTrace((whitespace | comment | newLine).rep)))
  val skipWs = P(NoCut(NoTrace(whitespace.rep)))

  val letter = P(CharIn(LettersRange))
  val digit  = P(CharIn(NumbersRange))
  val digits = P(digit.rep(1))
  val dash   = P(CharIn(Dashes.toList))

  val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")
  val strChars    = P(CharsWhile(StringChars))

  val hexDigit       = P(CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))
  val unicodeEsc     = P("u" ~ hexDigit.rep(4))
  val unicodeEscLong = P("U" ~ hexDigit.rep(8))
  val escape         = P("\\" ~ (
    CharIn("\"/\\bfnrt") | unicodeEsc | unicodeEscLong
  ))

  val basicStr: Parser[Value.Str] =
    P(DoubleQuote.toString ~/ (strChars | escape).rep.! ~ DoubleQuote.toString)
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
      newLine.? ~
      (!MultiLineDoubleQuote ~ AnyChar).rep.! ~
      MultiLineDoubleQuote
    ).map(str => Value.Str(Unescape.unescapeJavaString(str)))
  val multiLineLiteralStr: Parser[Value.Str] =
    P(
      MultiLineSingleQuote ~/
      newLine.? ~
      (!MultiLineSingleQuote ~ AnyChar).rep.! ~
      MultiLineSingleQuote
    ).map(Value.Str)

  val string: Parser[Value.Str] = P(
    multiLineBasicStr   |
    multiLineLiteralStr |
    basicStr            |
    literalStr)

  def rmUnderscore(s: String) = s.replace("_", "")

  val sign = P(CharIn("+-"))
  val integral = P(digits.rep(min = 1, sep = "_"))
  val fractional = P("." ~ integral)
  val exponent = P(CharIn("eE") ~ sign.? ~ integral)
  val integer: Parser[Value.Num] =
    P(sign.? ~ integral).!.map(s => Value.Num(rmUnderscore(s).toLong))
  val double: Parser[Value.Real] =
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

  val `true`  = P("true") .map(_ => Value.Bool(true))
  val `false` = P("false").map(_ => Value.Bool(false))
  val boolean = P(`true` | `false`)

  val bareKey = P((letter | digit | dash).rep(min = 1)).!
  val validKey: Parser[String] =
    P(NoCut(basicStr.map(_.value)) | NoCut(literalStr.map(_.value)) | bareKey)
  val pair: Parser[(String, Value)] =
    P(validKey ~ skipWs ~ "=" ~ skipWs ~ elem)
  val array: Parser[Value.Arr] =
    P("[" ~ skip ~ elem.rep(sep = skip ~ "," ~ skip) ~ ",".? ~ skip ~ "]")
      .map(l => Value.Arr(l.toList))
  val inlineTable: Parser[Value.Tbl] =
    (if (extensions.contains(MultiLineInlineTables))
      P("{" ~ skip ~ pair.rep(sep = skip ~ "," ~ skip) ~ ",".? ~ skip ~ "}")
     else
      P("{" ~ skipWs ~ pair.rep(sep = skipWs ~ "," ~ skipWs) ~ skipWs ~ "}")
    ).map(p => Value.Tbl(p.toMap))

  val tableIds: Parser[Seq[String]] =
    P(validKey.rep(min = 1, sep = skipWs ~ "." ~ skipWs).map(_.toSeq))
  val tableDef: Parser[Seq[String]] =
    P("[" ~ skipWs ~ tableIds ~ skipWs ~ "]")
  val tableArrayDef: Parser[Seq[String]] =
    P("[[" ~ skipWs ~ tableIds ~ skipWs ~ "]]")

  val pairNode: Parser[Node.Pair] = pair.map { case (k, v) => Node.Pair(k, v) }

  val variableIdent: Parser[String] = "$" ~ bareKey
  val variableAssign: Parser[Node.Variable] =
    (variableIdent ~ skipWs ~ "=" ~ skipWs ~ elem)
      .map { case (k, v) => Node.Variable(k, v) }
  val variableAccess: Parser[Value.Variable] =
    variableIdent.map(Value.Variable(_))

  val table: Parser[Node.NamedTable] =
    P(tableDef ~ skip ~ pair.rep(sep = skip)).map { case (a, b) =>
      Node.NamedTable(a.toList, b.toList)
    }
  val tableArray: Parser[Node.NamedArray] =
    P(tableArrayDef ~ skip ~ pair.rep(sep = skip)).map { case (a, b) =>
      Node.NamedArray(a.toList, b.toList)
    }

  // TODO left recursion
  val concat: Parser[Value.Concat] =
    P(elem ~ skipWs ~ "+" ~ skipWs ~ elem).map { case (l, r) =>
      Value.Concat(l, r)
    }

  lazy val elem: Parser[Value] =
    if (!extensions.contains(Variables))
      P(date | string | boolean | double | integer | array | inlineTable)
    else
      P(date | string | boolean | double | integer | array | inlineTable | variableAccess)

  val node: Parser[Node] =
    if (!extensions.contains(Variables))
      P(pairNode | table | tableArray)
    else
      P(pairNode | table | tableArray | variableAssign)

  val root: Parser[Root] = P(skip ~ node.rep(sep = skip) ~ skip ~ End)
    .map(nodes => Root(nodes.toList))
}
