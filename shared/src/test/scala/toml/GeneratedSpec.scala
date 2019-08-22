package toml

import org.scalatest.prop._
import org.scalatest.{Matchers, PropSpec}
import scala.meta.internal.fastparse.all._
import scala.meta.internal.fastparse.core.Parsed.{Failure, Success}

import scala.util.Try

class GeneratedSpec extends PropSpec with PropertyChecks with Matchers {
  import TestHelpers._

  property("Parse arrays") {
    import Generators.Arrays._
    forAll(arrayGen) { s: String =>
      shouldBeSuccess(Rules.elem.parse(s))
    }
  }

  property("Parse integers") {
    import Generators.Numbers._
    forAll(validLongGen) { s: String =>
      val expected = Success(Value.Num(Rules.rmUnderscore(s).toLong), s.length)
      Rules.elem.parse(s) shouldBe expected
    }
  }

  ignore("Parse doubles") {
    import Generators.Numbers._
    forAll(validDoubleGen) { s: String =>
      val expected = Success(Value.Real(Rules.rmUnderscore(s).toDouble), s.length)
      Rules.elem.parse(s) shouldBe expected
    }
  }

  property("Parse booleans") {
    import Generators.Booleans._

    forAll(validBoolGen) { s: String =>
      val expected = Success(toBool(s), s.length)
      Rules.elem.parse(s) shouldBe expected
    }
  }

  property("Detect if booleans are not lowercase") {
    import Generators.Booleans._
    forAll(invalidBoolGen) { s: String =>
      shouldBeFailure(Rules.elem.parse(s))
    }
  }

  property("Detect if any string is unbalanced (missing quote)") {
    import Generators.Strings._
    forAll(invalidStrGen) { s: String =>
      shouldBeFailure(Rules.elem.parse(s))
    }
  }

  property("Parse pairs (key and value)") {
    import Generators.Tables._
    forAll(pairGen) { s: String =>
      shouldBeSuccess[(String, Value)](Rules.pair.parse(s))
    }
  }

  property("Parse pairs (with `root` parser)") {
    import Generators.Tables._
    forAll(pairGen) { s: String =>
      shouldBeSuccess(Rules.root.parse(s))
    }
  }

  property("Parse table definitions") {
    import Generators.Tables._
    forAll(tableDefGen) { s: String =>
      shouldBeSuccess[Seq[String]](Rules.tableDef.parse(s))
    }
  }

  property("Parse tables") {
    import Generators.Tables._
    forAll(tableGen) { s: String =>
      shouldBeSuccess[Node.NamedTable]((Rules.skip ~ Rules.table).parse(s))
    }
  }

  property("Parse tables (with `root` parser)") {
    import Generators.Tables._
    forAll(tableGen) { s: String =>
      shouldBeSuccess(Rules.root.parse(s))
    }
  }
}
