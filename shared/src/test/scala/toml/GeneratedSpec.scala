package toml

import org.scalatest.prop._
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import fastparse._
import fastparse.Parsed.{Failure, Success}

import scala.util.Try

class GeneratedSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers {
  import TestHelpers._

  property("Parse arrays") {
    import Generators.Arrays._
    forAll(arrayGen) { s: String =>
      shouldBeSuccess(parse(s, Rules.elem(_)))
    }
  }

  property("Parse integers") {
    import Generators.Numbers._
    forAll(validLongGen) { s: String =>
      val expected = Success(Value.Num(Rules.rmUnderscore(s).toLong), s.length)
      parse(s, Rules.elem(_)) shouldBe expected
    }
  }

  ignore("Parse doubles") {
    import Generators.Numbers._
    forAll(validDoubleGen) { s: String =>
      val expected = Success(Value.Real(Rules.rmUnderscore(s).toDouble), s.length)
      parse(s, Rules.elem(_)) shouldBe expected
    }
  }

  property("Parse booleans") {
    import Generators.Booleans._

    forAll(validBoolGen) { s: String =>
      val expected = Success(toBool(s), s.length)
      parse(s, Rules.elem(_)) shouldBe expected
    }
  }

  property("Detect if booleans are not lowercase") {
    import Generators.Booleans._
    forAll(invalidBoolGen) { s: String =>
      shouldBeFailure(parse(s, Rules.elem(_)))
    }
  }

  property("Detect if any string is unbalanced (missing quote)") {
    import Generators.Strings._
    forAll(invalidStrGen) { s: String =>
      shouldBeFailure(parse(s, Rules.elem(_)))
    }
  }

  property("Parse pairs (key and value)") {
    import Generators.Tables._
    forAll(pairGen) { s: String =>
      shouldBeSuccess[(String, Value)](parse(s, Rules.pair(_)))
    }
  }

  property("Parse pairs (with `root` parser)") {
    import Generators.Tables._
    forAll(pairGen) { s: String =>
      shouldBeSuccess(parse(s, Rules.root(_)))
    }
  }

  property("Parse table definitions") {
    import Generators.Tables._
    forAll(tableDefGen) { s: String =>
      shouldBeSuccess[Seq[String]](parse(s, Rules.tableDef(_)))
    }
  }

  property("Parse tables") {
    import NoWhitespace._
    import Generators.Tables._
    def p[$: P] = P(Rules.skip ~ Rules.table)
    forAll(tableGen) { s: String =>
      shouldBeSuccess[Node.NamedTable](parse(s, p(_)))
    }
  }

  property("Parse tables (with `root` parser)") {
    import Generators.Tables._
    forAll(tableGen) { s: String =>
      shouldBeSuccess(parse(s, Rules.root(_)))
    }
  }
}
