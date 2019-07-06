package toml

import org.scalatest.prop._
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.meta.internal.fastparse.all._
import scala.meta.internal.fastparse.core.Parsed.{Failure, Success}

class DateGenSpec extends PropSpec with ScalaCheckPropertyChecks with Matchers {
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
