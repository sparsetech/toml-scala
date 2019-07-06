package toml

import org.scalacheck.Gen
import org.scalacheck.Gen._

object Generators {
  import Constants._

  object Strings {
    def enquoteStr(s: String, q: Char): String = q + s + q

    def quotedStrGen(quote: Char): Gen[String] = for {
      s <- Gen.alphaStr if s.nonEmpty
    } yield enquoteStr(s.filter(_ != quote), quote)

    def doubleQuoteStrGen: Gen[String] = quotedStrGen(DoubleQuote)
    def singleQuoteStrGen: Gen[String] = quotedStrGen(SingleQuote)
    def validStrGen: Gen[String] =
      Gen.oneOf(doubleQuoteStrGen, singleQuoteStrGen)

    def invalidStrGen: Gen[String] = for {
      s <- Gen.alphaStr
      f <- Gen.oneOf(
        s"$SingleQuote$s",
        s + SingleQuote,
        s"$DoubleQuote$s",
        s + DoubleQuote
      )
    } yield f
  }

  object Numbers {
    val signChars = List("+", "-", "")

    def intersperse(x: String, sep: String) = x.toCharArray.mkString(sep)

    def validLongGen: Gen[String] = for {
      l    <- Gen.posNum[Long]
      sign <- Gen.oneOf(signChars)
      us   <- Gen.oneOf(List("", "_"))
    } yield sign + intersperse(l.toString, us)

    def validDoubleGen: Gen[String] = for {
      sign <- Gen.oneOf(signChars)
      d    <- Gen.posNum[Double]
      l    <- validLongGen
      l2   <- validLongGen
      e    <- Gen.oneOf("e", "E")
      fs   <- Gen.oneOf(sign + d.toString, l + e + l2)
    } yield fs
  }

  object Booleans {
    def toBool(s: String) = s match {
      case "true"  => Value.Bool(true)
      case "false" => Value.Bool(false)
      case x       => sys.error(s"$x is not either true or false.")
    }

    def validBoolGen  : Gen[String] = Gen.oneOf("true", "false")
    def invalidBoolGen: Gen[String] = Gen.oneOf("True", "False")
  }

  object Comments {
    /* Generate comments in a low probability */
    def commentGen: Gen[String] = for {
      strChunk <- Gen.alphaStr
      rand     <- Gen.chooseNum(1, 10)
      nl       <- Gen.oneOf(CrLf, Lf)
    } yield if (rand <= 3) "#" + strChunk + nl else ""
  }

  object Arrays {
    import Strings._
    import Numbers._

    val openChars  = List("[", "[\n")
    val seps       = List(",\n", ",")
    val closeChars = List("]", "\n]")

    def arrayFormat(s: Seq[_], fs: (String, String, String)): String =
      fs._1 + s.mkString(fs._2) + fs._3

    def arrayGen = for {
      ts <- oneOf(validStrGen, validDoubleGen, validLongGen)
      elems <- nonEmptyListOf(ts)
      c1 <- oneOf(openChars)
      ss <- oneOf(seps)
      c2 <- oneOf(closeChars)
    } yield arrayFormat(elems, (c1, ss, c2))
  }

  object Dates {
    def pad(n: Int, s: String): String =
      if(s.length < n) ("0" * (n - s.length)) + s else s
    def genNum(digits: Int, from: Int, to: Int): Gen[String] =
      chooseNum(from, to).map(n => pad(digits, n.toString))

    /* This check is not covering all the formats */
    val dateFormatGen: Gen[String] = for {
      day <- genNum(2, 1, 28)
      month <- genNum(2, 1, 12)
      year <- genNum(4, 0, 2200)
      hour <- genNum(2, 0, 23)
      minute <- genNum(2, 0, 59)
      second <- genNum(2, 0, 59)
      micro <- genNum(3, 0, 999)
    } yield year + "-" + month + "-" + day +
      "T" + hour + ":" + minute + ":" +
      second + "." + micro + "Z"
  }

  object Tables {
    import Strings._
    import Numbers._
    import Booleans._
    import Comments._

    val sps = List(" ", "\t")

    def pairFormat(l: String, r: String, sp: String) = l + sp + "=" + sp + r

    def idTableFormat(ids: Seq[_], fs: (String, String, String)) =
      fs._1 + ids.mkString(fs._2) + fs._3

    def tableFormat(id: String, ps: Seq[String]) =
      id + "\n" + ps.mkString("\n")

    def valueGen: Gen[String] = oneOf(
      validStrGen,
      validDoubleGen,
      validLongGen,
      validBoolGen
    ).suchThat(_.nonEmpty)

    def bareKeyGen = Gen.someOf(
      Gen.alphaLowerChar,
      Gen.alphaUpperChar,
      Gen.alphaNumChar,
      Gen.oneOf('_', '-')
    ).suchThat(_.nonEmpty).map(_.mkString)

    def pairGen: Gen[String] = for {
      key   <- oneOf(doubleQuoteStrGen, bareKeyGen)
      value <- valueGen
      sp    <- oneOf(sps)
      i     <- chooseNum(0, 5)
    } yield pairFormat(key, value, sp * i)

    def pairWithCommentsGen: Gen[String] = for {
      p                  <- pairGen
      commInPreviousLine <- commentGen
      commInSameLine     <- commentGen
    } yield commInPreviousLine + p + commInSameLine

    def tableDefGen: Gen[String] = for {
      labels <- nonEmptyListOf(doubleQuoteStrGen)
      sp     <- oneOf(sps)
      i      <- chooseNum(0, 10)
      sep    <- const(sp * i + "." + sp * i)
    } yield idTableFormat(labels, ("[", sep , "]"))

    def tableGen = for {
      tdef <- tableDefGen
      n <- chooseNum(0, 10)
      ps <- listOfN(n, pairWithCommentsGen)
      c1 <- commentGen
      c2 <- commentGen
    } yield c1 + tableFormat(tdef, ps) + c2
  }
}
