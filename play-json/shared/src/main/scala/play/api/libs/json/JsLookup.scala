/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

/**
 * A value representing the value at a particular JSON path, either an actual JSON node or undefined.
 */
case class JsLookup(result: JsLookupResult) extends AnyVal {

  /**
   * Access the head of this array.
   */
  def head: JsLookupResult = result match {
    case JsDefined(JsArray(head +: tail)) => JsDefined(head)
    case JsDefined(arr @ JsArray(_))      => JsUndefined(s"Cannot get head of $arr")
    case JsDefined(o)                     => JsUndefined(s"$o is not an array")
    case undef                            => undef
  }

  /**
   * Access the tail of this array.
   */
  def tail: JsLookupResult = result match {
    case JsDefined(JsArray(head +: tail)) => JsDefined(JsArray(tail))
    case JsDefined(arr @ JsArray(_))      => JsUndefined(s"Cannot get tail of $arr")
    case JsDefined(o)                     => JsUndefined(s"$o is not an array")
    case undef                            => undef
  }

  /**
   * Access the last element of this array.
   */
  def last: JsLookupResult = result match {
    case JsDefined(JsArray(values)) if values.nonEmpty => JsDefined(values.last)
    case JsDefined(arr @ JsArray(_))                   => JsUndefined(s"Cannot get last element of $arr")
    case JsDefined(o)                                  => JsUndefined(s"$o is not an array")
    case undef                                         => undef
  }

  /**
   * Access a value of this array.
   *
   * @param index Element index.
   */
  def apply(index: Int): JsValue = result match {
    case JsDefined(x) =>
      x match {
        case arr: JsArray =>
          arr.value.lift(index) match {
            case Some(x) => x
            case None    => throw new IndexOutOfBoundsException(String.valueOf(index))
          }
        case _ =>
          throw new Exception(s"$x is not a JsArray")
      }
    case x: JsUndefined =>
      throw new Exception(String.valueOf(x.error))
  }

  /**
   * Access a value of this array.
   *
   * @param fieldName Element index.
   */
  def apply(fieldName: String): JsValue = result match {
    case JsDefined(x) =>
      x match {
        case arr: JsObject =>
          arr.underlying.get(fieldName) match {
            case Some(x) => x
            case None    => throw new NoSuchElementException(String.valueOf(fieldName))
          }
        case _ =>
          throw new Exception(s"$x is not a JsObject")
      }
    case x: JsUndefined =>
      throw new Exception(String.valueOf(x.error))
  }

  /**
   * Access a value of this array.
   *
   * @param index Element index
   */
  def \(index: Int): JsLookupResult = result match {
    case JsDefined(arr: JsArray) =>
      arr.value.lift(index).map(JsDefined.apply).getOrElse(JsUndefined(s"Array index out of bounds in $arr"))
    case JsDefined(o) =>
      JsUndefined(s"$o is not an array")
    case undef => undef
  }

  /**
   * Return the property corresponding to the fieldName, supposing we have a JsObject.
   *
   * @param fieldName the name of the property to look up
   * @return the resulting JsValue wrapped in a JsLookup. If the current node is not a JsObject or doesn't have the
   *         property, a JsUndefined will be returned.
   */
  def \(fieldName: String): JsLookupResult = result match {
    case JsDefined(obj @ JsObject(_)) =>
      obj.underlying
        .get(fieldName)
        .map(JsDefined.apply)
        .getOrElse(JsUndefined(s"'$fieldName' is undefined on object: $obj"))
    case JsDefined(o) =>
      JsUndefined(s"$o is not an object")
    case undef => undef
  }

  /**
   * Look up fieldName in the current object and all descendants.
   *
   * @return the list of matching nodes
   */
  def \\(fieldName: String): collection.Seq[JsValue] = result match {
    case JsDefined(obj: JsObject) =>
      obj.underlying.foldLeft(Seq[JsValue]())((o, pair) =>
        pair match {
          case (key, value) if key == fieldName => o ++ (value +: (value \\ fieldName))
          case (_, value)                       => o ++ (value \\ fieldName)
        }
      )

    case JsDefined(arr: JsArray) =>
      arr.value.flatMap(_ \\ fieldName)

    case _ => Seq.empty
  }
}

sealed trait JsLookupResult extends Any with JsReadable {

  /**
   * Tries to convert the node into a JsValue
   */
  def toOption: Option[JsValue] = this match {
    case JsDefined(v) => Some(v)
    case _            => None
  }

  def toEither: Either[JsonValidationError, JsValue] = this match {
    case JsDefined(v)       => Right(v)
    case undef: JsUndefined => Left(undef.validationError)
  }

  def get: JsValue = toOption.get

  def getOrElse(v: => JsValue): JsValue = toOption.getOrElse(v)

  def isEmpty: Boolean = this match {
    case _: JsUndefined => true
    case JsDefined(_)   => false
  }

  def isDefined: Boolean = !isEmpty

  /**
   * If this result is defined return `this`. Otherwise return `alternative`.
   */
  def orElse(alternative: => JsLookupResult): JsLookupResult =
    if (isDefined) this else alternative

  def validate[A](implicit
      rds: Reads[A]
  ): JsResult[A] = this match {
    case JsDefined(v)       => v.validate[A]
    case undef: JsUndefined => JsError(undef.validationError)
  }

  /**
   * If this result contains `JsNull` or is undefined, returns `JsSuccess(None)`.
   * Otherwise returns the result of validating as an `A` and wrapping the result in a `Some`.
   */
  def validateOpt[A](implicit
      rds: Reads[A]
  ): JsResult[Option[A]] =
    this match {
      case JsDefined(a) => Reads.optionWithNull(rds).reads(a)
      case _            => JsSuccess(None)
    }
}

object JsLookupResult {
  import scala.language.implicitConversions
  implicit def jsLookupResultToJsLookup(value: JsLookupResult): JsLookup = JsLookup(value)
  private[json] val PathMissing                                          = JsUndefined("error.path.missing")
}

/**
 * Wrapper for JsValue to represent an existing Json value.
 */
case class JsDefined(value: JsValue) extends AnyVal with JsLookupResult

/**
 * Represent a missing Json value.
 */
final class JsUndefined(err: => String) extends JsLookupResult {
  def error             = err
  def validationError   = JsonValidationError(error)
  override def toString = s"JsUndefined($err)"
}

object JsUndefined {
  def apply(err: => String)       = new JsUndefined(err)
  def unapply(o: Object): Boolean = o.isInstanceOf[JsUndefined]
}
