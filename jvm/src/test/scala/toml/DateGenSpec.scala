package toml

import org.scalatest.prop._
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import fastparse._
import fastparse.Parsed.{Failure, Success}

class DateGenSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers {
  import TestHelpers._

  property("parse dates following the RFC 3339 spec (`date` parser)") {
    import Generators.Dates._
    forAll(dateFormatGen) { s =>
      shouldBeSuccess(parse(s, Rules.offsetDateTime(_)))
    }
  }

  property("parse dates following the RFC 3339 spec") {
    import Generators.Dates._
    forAll(dateFormatGen) { s =>
      shouldBeSuccess(parse(s, Rules.elem(_)))
    }
  }
}
