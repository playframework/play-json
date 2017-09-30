/*
 * Copyright (C) 2009-2017 Lightbend Inc. <https://www.lightbend.com>
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
    case JsDefined(arr @ JsArray(_)) => JsUndefined(s"Cannot get head of $arr")
    case JsDefined(o) => JsUndefined(s"$o is not an array")
    case undef => undef
  }

  /**
   * Access the tail of this array.
   */
  def tail: JsLookupResult = result match {
    case JsDefined(JsArray(head +: tail)) => JsDefined(JsArray(tail))
    case JsDefined(arr @ JsArray(_)) => JsUndefined(s"Cannot get tail of $arr")
    case JsDefined(o) => JsUndefined(s"$o is not an array")
    case undef => undef
  }

  /**
   * Access the last element of this array.
   */
  def last: JsLookupResult = result match {
    case JsDefined(JsArray(values)) if values.nonEmpty => JsDefined(values.last)
    case JsDefined(arr @ JsArray(_)) => JsUndefined(s"Cannot get last element of $arr")
    case JsDefined(o) => JsUndefined(s"$o is not an array")
    case undef => undef
  }

  /**
   * Access a value of this array.
   *
   * @param index Element index.
   */
  def apply(index: Int): JsValue = result match {
    case JsDefined(x) => x match {
      case arr: JsArray => arr.value.lift(index) match {
        case Some(x) => x
        case None => throw new IndexOutOfBoundsException(String.valueOf(index))
      }
      case _ =>
        throw new Exception(x + " is not a JsArray")
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
    case JsDefined(x) => x match {
      case arr: JsObject => arr.value.lift(fieldName) match {
        case Some(x) => x
        case None => throw new NoSuchElementException(String.valueOf(fieldName))
      }
      case _ =>
        throw new Exception(x + " is not a JsObject")
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
      obj.value.get(fieldName).map(JsDefined.apply)
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
  def \\(fieldName: String): Seq[JsValue] = result match {
    case JsDefined(obj: JsObject) =>
      obj.value.foldLeft(Seq[JsValue]())((o, pair) => pair match {
        case (key, value) if key == fieldName => o ++ (value +: (value \\ fieldName))
        case (_, value) => o ++ (value \\ fieldName)
      })

    case JsDefined(arr: JsArray) =>
      arr.value.flatMap(_ \\ fieldName)

    case _ => Seq.empty
  }
}