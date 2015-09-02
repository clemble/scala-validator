package com.clemble.util.validator

import org.specs2.mutable.Specification
import play.api.libs.json.Json
import com.clemble.util.validator.PatchValidator._

/**
 * Check case class with basic arrays fields, string, integer & boolean.
 */
class ArrayCaseClassValidatorSpec extends Specification {

  case class ArrayCaseClass(
     strArr: List[String],
     numArr: Seq[Int],
     boolArr: Array[Boolean]
   )

  object ArrayCaseClass {

    implicit val format = Json.format[ArrayCaseClass]

    implicit val validator = PatchValidator.validator[ArrayCaseClass]

  }

  "Array case class" should {

    "validate string array" in {
      isValid[ArrayCaseClass](Json.obj("strArr" -> Json.arr())) shouldEqual true
      isValid[ArrayCaseClass](Json.obj("strArr" -> Json.arr("Some string"))) shouldEqual true
      isValid[ArrayCaseClass](Json.obj("strArr" -> Json.arr(1.1))) shouldEqual false
      isValid[ArrayCaseClass](Json.obj("strArr" -> Json.arr(true))) shouldEqual false
      isValid[ArrayCaseClass](Json.obj("strArr" -> Json.arr("Some string", 1, true))) shouldEqual false
    }

    "validate number array" in {
      isValid[ArrayCaseClass](Json.obj("numArr" -> Json.arr())) shouldEqual true
      isValid[ArrayCaseClass](Json.obj("numArr" -> Json.arr(1))) shouldEqual true
      isValid[ArrayCaseClass](Json.obj("numArr" -> Json.arr("Some string"))) shouldEqual false
      isValid[ArrayCaseClass](Json.obj("numArr" -> Json.arr(true))) shouldEqual false
      isValid[ArrayCaseClass](Json.obj("strArr" -> Json.arr(1, "Some string", true))) shouldEqual false
    }

    "validate boolean array" in {
      isValid[ArrayCaseClass](Json.obj("boolArr" -> Json.arr())) shouldEqual true
      isValid[ArrayCaseClass](Json.obj("boolArr" -> Json.arr(true))) shouldEqual true
      isValid[ArrayCaseClass](Json.obj("boolArr" -> Json.arr(1))) shouldEqual false
      isValid[ArrayCaseClass](Json.obj("boolArr" -> Json.arr("Some string"))) shouldEqual false
      isValid[ArrayCaseClass](Json.obj("strArr" -> Json.arr(true, 1, "Some string"))) shouldEqual false
    }

    "invalidate extra fields" in {
      isValid[ArrayCaseClass](Json.obj("BoolArr" -> Json.arr(true))) shouldEqual false
      isValid[ArrayCaseClass](Json.obj("StrArr" -> Json.arr("Some string"))) shouldEqual false
      isValid[ArrayCaseClass](Json.obj("NumArr" -> Json.arr(1))) shouldEqual false
      isValid[ArrayCaseClass](Json.obj("FakeArr" -> Json.arr(1))) shouldEqual false
    }

  }

}
