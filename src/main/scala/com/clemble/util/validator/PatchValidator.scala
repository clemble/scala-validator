package com.clemble.util.validator

import play.api.libs.json.{JsObject, Reads, JsValue, __}
import shapeless._
import shapeless.labelled.FieldType

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

object PatchValidator extends PatchValidatorLowImplicits {

  /**
   * In case of empty HNil, ensure Json is empty as well, to prevent adding
   * trash to JSON.
   *
   * @return tail PatchValidator
   */
  implicit def deriveHNil: PatchValidator[HNil] =
    new PatchValidator[HNil] {
      def isValid(json: JsValue): Boolean = {
        json match {
          case JsObject(fields) => fields.isEmpty
          case _ => true
        }
      }
    }

  /**
   * Derive a case class field using a `PatchValidator`
   */
  implicit def deriveHCons[K <: Symbol, V, T <: HList]
    (implicit
      key: Witness.Aux[K],
      sv: Lazy[PatchValidator[V]],
      st: Lazy[PatchValidator[T]]
    ): PatchValidator[FieldType[K, V] :: T] =
    new PatchValidator[FieldType[K, V] :: T] {
      def isValid(json: JsValue): Boolean = {
        json match {
          case obj: JsObject =>
            val field = key.value.name
            (__ \ field).readNullable[JsValue].filter(_.forall(sv.value.isValid)).reads(obj).isSuccess && st.value.isValid(obj - field)
          case _ => true
        }
      }
    }

  class PatchValidatorGen[T] {
    def validator[TT](implicit gen: LabelledGeneric.Aux[T, TT], vtt: Lazy[PatchValidator[TT]]): PatchValidator[T] =
      new PatchValidator[T] {
        override def isValid(json: JsValue): Boolean = vtt.value.isValid(json)
      }
  }

  /**
   * Validator factory method
   *
   * @tparam T entity type
   * @return PatchValidator for provided entity
   */
  def apply[T] = new PatchValidatorGen[T]

  /**
   * Check JSON is valid partial presentation of type `T`
   *
   * @param json partial json presentation
   * @param validator validator to use
   * @tparam T type
   * @return
   */
  def isValid[T](json: JsValue)(implicit validator: PatchValidator[T]) = {
    validator.isValid(json)
  }


}

trait PatchValidatorLowImplicits {

  /**
   * Derive a case class field using a `Reads`
   */
  implicit def deriveHConsReads[K <: Symbol, V, T <: HList]
    (implicit
      key: Witness.Aux[K],
      rv: Lazy[Reads[V]],
      st: Lazy[PatchValidator[T]]
    ): PatchValidator[FieldType[K, V] :: T] =
    new PatchValidator[FieldType[K, V] :: T] {
      def isValid(json: JsValue): Boolean = {
        json match {
          case obj: JsObject =>
            val field = key.value.name
            (__ \ field).readNullable(rv.value).reads(obj).isSuccess && st.value.isValid(obj - field)
          case _ => false
        }
      }
    }

}
