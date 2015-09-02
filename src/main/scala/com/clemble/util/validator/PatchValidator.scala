package com.clemble.util.validator

import play.api.libs.json.JsValue

/**
 * Partial JSON validator for entity, that allows to make partial object presentation validation.
 * @tparam T entity type
 */
trait PatchValidator[T] {

  /**
   * Checks if this JSON is valid part of JSON presentation for provided
   *
   * @param json partial presentation to check
   * @return true if presentation is valid, false otherwise
   */
  def isValid(json: JsValue): Boolean

}

object PatchValidator {

  /**
   * Validator factory method
   *
   * @tparam T entity type
   * @return PatchValidator for provided entity
   */
  def validator[T]():PatchValidator[T] = {
    new PatchValidator[T] {
      /**
       * Checks if this JSON is valid part of JSON presentation for provided
       *
       * @param json partial presentation to check
       * @return true if presentation is valid, false otherwise
       */
      override def isValid(json: JsValue): Boolean = false
    }
  }

  def isValid[T](json: JsValue)(implicit validator: PatchValidator[T]) = {
    validator.isValid(json)
  }


}
