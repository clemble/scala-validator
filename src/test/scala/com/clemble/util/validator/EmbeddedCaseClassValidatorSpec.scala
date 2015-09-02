package com.clemble.util.validator

import org.specs2.mutable.Specification
import play.api.libs.json.Json

/**
 * Created by mavarazy on 9/2/15.
 */
class EmbeddedCaseClassValidatorSpec extends Specification {

  case class Phone(
    extension: String,
    phone: String
  )

  case class Contact(
    name: String,
    phone: Phone
  )

  object Contact {

    implicit val phoneFormat = Json.format[Phone]

    implicit val contactFormat = Json.format[Contact]

    implicit val phoneValidator = PatchValidator.validator[Phone]

    implicit val validator = PatchValidator.validator[Contact]

  }

  "Embedded class" should {

    import PatchValidator._

    "validate partial presentation of extension field" in {
      isValid[Contact](Json.obj("phone" -> Json.obj("extension" -> "02"))) shouldEqual true
      isValid[Contact](Json.obj("phone" -> Json.obj("extension" -> 2))) shouldEqual false
      isValid[Contact](Json.obj("phone" -> Json.obj("extension" -> true))) shouldEqual false
      isValid[Contact](Json.obj("phone" -> Json.obj("extension" -> Json.arr()))) shouldEqual false
    }

    "validate partial presentation of phone field" in {
      isValid[Contact](Json.obj("phone" -> Json.obj("phone" -> "01213122"))) shouldEqual true
      isValid[Contact](Json.obj("phone" -> Json.obj("phone" -> 231241412))) shouldEqual false
      isValid[Contact](Json.obj("phone" -> Json.obj("phone" -> true))) shouldEqual false
      isValid[Contact](Json.obj("phone" -> Json.obj("phone" -> Json.arr()))) shouldEqual false
    }

    "validate partial presentation of full object" in {
      isValid[Contact](Json.obj("phone" -> Json.obj("extension" -> "02", "phone" -> "01213122"))) shouldEqual true
      isValid[Contact](Json.obj("phone" -> Json.obj("extension" -> "02", "phone" -> 231241412))) shouldEqual false
      isValid[Contact](Json.obj("phone" -> Json.obj("extension" -> "02", "phone" -> true))) shouldEqual false
      isValid[Contact](Json.obj("phone" -> Json.obj("extension" -> "02", "phone" -> Json.arr()))) shouldEqual false

      isValid[Contact](Json.obj("phone" -> Json.obj("phone" -> "01213122", "extension" -> "02"))) shouldEqual true
      isValid[Contact](Json.obj("phone" -> Json.obj("phone" -> "01213122", "extension" -> 2))) shouldEqual false
      isValid[Contact](Json.obj("phone" -> Json.obj("phone" -> "01213122", "extension" -> true))) shouldEqual false
      isValid[Contact](Json.obj("phone" -> Json.obj("phone" -> "01213122", "extension" -> Json.arr()))) shouldEqual false
    }

    "invalidate extra fields in embedded object" in {
      isValid[Contact](Json.obj("phone" -> Json.obj("extension" -> "02", "phone" -> "01213122", "extraStr" -> "some"))) shouldEqual false
      isValid[Contact](Json.obj("phone" -> Json.obj("extension" -> "02", "phone" -> "01213122", "extraNum" -> 1))) shouldEqual false
      isValid[Contact](Json.obj("phone" -> Json.obj("extension" -> "02", "phone" -> "01213122", "extraBool" -> false))) shouldEqual false
      isValid[Contact](Json.obj("phone" -> Json.obj("extension" -> "02", "phone" -> "01213122", "extraArr" -> Json.arr()))) shouldEqual false
    }

  }

}
