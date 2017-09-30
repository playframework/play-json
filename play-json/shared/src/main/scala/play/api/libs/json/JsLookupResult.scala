/*
 * Copyright (C) 2009-2017 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

sealed trait JsLookupResult extends Any with JsReadable {
  /**
   * Tries to convert the node into a JsValue
   */
  def toOption: Option[JsValue]

  def toEither: Either[JsonValidationError, JsValue]

  def get: JsValue = toOption.get

  def getOrElse(v: => JsValue): JsValue = toOption.getOrElse(v)
  def isEmpty: Boolean
  def isDefined: Boolean = !isEmpty

  /**
   * If this result is defined return `this`. Otherwise return `alternative`.
   */
  def orElse(alternative: => JsLookupResult): JsLookupResult =
    if (isDefined) this else alternative

  /**
   * If this result contains `JsNull` or is undefined, returns `JsSuccess(None)`.
   * Otherwise returns the result of validating as an `A` and wrapping the result in a `Some`.
   */
  def validateOpt[A](implicit rds: Reads[A]): JsResult[Option[A]]

}
object JsLookupResult {
  import scala.language.implicitConversions
  implicit def jsLookupResultToJsLookup(value: JsLookupResult): JsLookup = JsLookup(value)
}

/**
 * Wrapper for JsValue to represent an existing Json value.
 */
case class JsDefined(value: JsValue) extends AnyVal with JsLookupResult {

  def toOption: Option[JsValue] = Some(value)

  def toEither: Either[JsonValidationError, JsValue] = Right(value)

  def isEmpty: Boolean = false

  def validate[A](implicit rds: Reads[A]): JsResult[A] = value.validate[A]

  def validateOpt[A](implicit rds: Reads[A]): JsResult[Option[A]] = Reads.optionWithNull(rds).reads(value)

}

/**
 * Represent a missing Json value.
 */
final class JsUndefined(err: => String) extends JsLookupResult {

  val toOption: Option[JsValue] = None

  val toEither: Either[JsonValidationError, JsValue] = Left(validationError)

  override def asOpt[T](implicit fjs: Reads[T]): Option[T] = None

  val isEmpty: Boolean = true

  def validate[A](implicit rds: Reads[A]): JsResult[A] = JsError(validationError)

  def validateOpt[A](implicit rds: Reads[A]): JsResult[Option[A]] = JsSuccess(None)

  def error = err
  def validationError = JsonValidationError(error)
  override def toString = s"JsUndefined($err)"
}

object JsUndefined {
  def apply(err: => String) = new JsUndefined(err)
  def unapply(o: Object): Boolean = o.isInstanceOf[JsUndefined]
}