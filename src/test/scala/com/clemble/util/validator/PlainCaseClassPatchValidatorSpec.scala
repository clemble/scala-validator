package com.clemble.util.validator

import org.specs2.mutable.Specification
import play.api.libs.json.{Format, Json}

/**
 * Check simple case class with basic fields, string, integer & boolean.
 */
// TODO add test for long & integer with checking upper & lower limits of integer results
class PlainCaseClassPatchValidatorSpec extends Specification {

  case class PlainCaseClass(
    str: String,
    numInt: Int,
    bool: Boolean
  )

  object PlainCaseClass {

    implicit val format: Format[PlainCaseClass] = Json.format[PlainCaseClass]

    implicit val validator: PatchValidator[PlainCaseClass] = PatchValidator[PlainCaseClass].validator

  }

  "Plain case class" should {

    import PatchValidator._

    "validate string" in {
      isValid[PlainCaseClass](Json.obj("str" -> "Some string")) shouldEqual true
      isValid[PlainCaseClass](Json.obj("str" -> 1.1)) shouldEqual false
      isValid[PlainCaseClass](Json.obj("str" -> true)) shouldEqual false
      isValid[PlainCaseClass](Json.obj("str" -> Json.arr())) shouldEqual false
    }

    "validate number" in {
      isValid[PlainCaseClass](Json.obj("numInt" -> 1)) shouldEqual true
      isValid[PlainCaseClass](Json.obj("numInt" -> "Some string")) shouldEqual false
      isValid[PlainCaseClass](Json.obj("numInt" -> true)) shouldEqual false
      isValid[PlainCaseClass](Json.obj("numInt" -> Json.arr())) shouldEqual false
    }

    "validate boolean" in {
      isValid[PlainCaseClass](Json.obj("bool" -> true)) shouldEqual true
      isValid[PlainCaseClass](Json.obj("bool" -> 1)) shouldEqual false
      isValid[PlainCaseClass](Json.obj("bool" -> "Some string")) shouldEqual false
      isValid[PlainCaseClass](Json.obj("bool" -> Json.arr())) shouldEqual false
    }

    "invalidate extra fields" in {
      isValid[PlainCaseClass](Json.obj("Str" -> "Some string")) shouldEqual false
      isValid[PlainCaseClass](Json.obj("NumInt" -> 1)) shouldEqual false
      isValid[PlainCaseClass](Json.obj("Bool" -> true)) shouldEqual false
      isValid[PlainCaseClass](Json.obj("Fake" -> true)) shouldEqual false
    }

  }

}
