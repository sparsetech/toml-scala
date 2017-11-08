package toml

import org.scalatest.prop._
import org.scalatest.{Matchers, PropSpec}

import fastparse.all._
import fastparse.core.Parsed.{Failure, Success}

class DateGenSpec extends PropSpec with PropertyChecks with Matchers {
  import TestHelpers._

  property("parse dates following the RFC 3339 spec (`date` parser)") {
    import Generators.Dates._
    forAll(dateFormatGen) { s =>
      shouldBeSuccess(Rules.offsetDateTime.parse(s))
    }
  }

  property("parse dates following the RFC 3339 spec") {
    import Generators.Dates._
    forAll(dateFormatGen) { s =>
      shouldBeSuccess(Rules.elem.parse(s))
    }
  }
}
