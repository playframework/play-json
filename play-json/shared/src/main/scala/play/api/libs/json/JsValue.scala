/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.collection._
import scala.util.hashing.MurmurHash3

case class JsResultException(errors: collection.Seq[(JsPath, collection.Seq[JsonValidationError])])
    extends RuntimeException(s"JsResultException(errors:$errors)")

/**
 * Generic json value
 */
sealed trait JsValue extends JsReadable {
  def validate[A](implicit rds: Reads[A]): JsResult[A] =
    rds.reads(this)

  def validateOpt[A](implicit rds: Reads[A]): JsResult[Option[A]] =
    JsDefined(this).validateOpt[A]

  override def toString = Json.stringify(this)
}

object JsValue {
  import scala.language.implicitConversions
  implicit def jsValueToJsLookup(value: JsValue): JsLookup =
    JsLookup(JsDefined(value))
}

/**
 * Represents a Json null value.
 */
case object JsNull extends JsValue {
  @transient
  implicit val reads: Reads[JsNull.type] = Reads[JsNull.type] {
    case JsNull => JsSuccess(JsNull)
    case _      => JsError("error.expected.null")
  }
}

/**
 * Represents a Json boolean value.
 */
sealed abstract class JsBoolean(val value: Boolean) extends JsValue with Product with Serializable {
  def canEqual(that: Any): Boolean = that.isInstanceOf[JsBoolean]

  override def equals(that: Any): Boolean =
    canEqual(that) && (this.value == that.asInstanceOf[JsBoolean].value)

  override def hashCode: Int = value.hashCode
}

/**
 * Represents Json Boolean True value.
 */
case object JsTrue extends JsBoolean(true)

/**
 * Represents Json Boolean False value.
 */
case object JsFalse extends JsBoolean(false)

object JsBoolean extends (Boolean => JsBoolean) {
  def apply(value: Boolean): JsBoolean = if (value) JsTrue else JsFalse

  def unapply(b: JsBoolean): Some[Boolean] = Some(b.value)
}

/**
 * Represent a Json number value.
 */
case class JsNumber(value: BigDecimal) extends JsValue

/**
 * Represent a Json string value.
 */
case class JsString(value: String) extends JsValue

/**
 * Represent a Json array value.
 */
case class JsArray(value: IndexedSeq[JsValue] = Array[JsValue]()) extends JsValue {
  // keeping this method will also help bincompat with older play-json versions
  private[json] def this(value: collection.Seq[JsValue]) = this(value.toArray[JsValue])

  /**
   * Concatenates this array with the elements of an other array.
   */
  def ++(other: JsArray): JsArray =
    JsArray(value ++ other.value)

  /**
   * Append an element to this array.
   */
  def :+(el: JsValue): JsArray     = JsArray(value :+ el)
  def append(el: JsValue): JsArray = this.:+(el)

  /**
   * Prepend an element to this array.
   */
  def +:(el: JsValue): JsArray      = JsArray(el +: value)
  def prepend(el: JsValue): JsArray = this.+:(el)
}

object JsArray extends (IndexedSeq[JsValue] => JsArray) {
  def apply(value: collection.Seq[JsValue]) = new JsArray(value.toArray[JsValue])

  def empty = JsArray(Array.empty[JsValue])
}

/**
 * Represent a Json object value.
 */
case class JsObject(
    private[json] val underlying: Map[String, JsValue]
) extends JsValue {

  /**
   * The fields of this JsObject in the order passed to the constructor
   */
  def fields: collection.Seq[(String, JsValue)] = underlying.toSeq

  /**
   * The value of this JsObject as an immutable map.
   */
  def value: Map[String, JsValue] = underlying match {
    case m: immutable.Map[String, JsValue] => m
    case m                                 => JsObject.createFieldsMap(m)
  }

  /**
   * Return all fields as a set
   */
  def fieldSet: Set[(String, JsValue)] = underlying.toSet

  /**
   * Return all keys
   */
  def keys: Set[String] = underlying.keySet

  /**
   * Return all values
   */
  def values: Iterable[JsValue] = underlying.values

  /**
   * Merge this object with another one. Values from other override value of the current object.
   */
  def ++(other: JsObject): JsObject = JsObject(underlying ++ other.underlying)

  /**
   * Removes one field from the JsObject
   */
  def -(otherField: String): JsObject = JsObject(underlying - otherField)

  /**
   * Adds one field to the JsObject
   */
  def +(otherField: (String, JsValue)): JsObject = JsObject(underlying + otherField)

  /**
   * merges everything in depth and doesn't stop at first level, as ++ does
   */
  def deepMerge(other: JsObject): JsObject = {
    def merge(existingObject: JsObject, otherObject: JsObject): JsObject = {
      val result = existingObject.underlying ++ otherObject.underlying.map { case (otherKey, otherValue) =>
        val maybeExistingValue = existingObject.underlying.get(otherKey)

        val newValue = (maybeExistingValue, otherValue) match {
          case (Some(e: JsObject), o: JsObject) => merge(e, o)
          case _                                => otherValue
        }
        otherKey -> newValue
      }
      JsObject(result)
    }
    merge(this, other)
  }

  override def equals(other: Any): Boolean = {
    other match {
      case o: AnyRef if this.eq(o) =>
        true
      case JsObject(that) =>
        underlying == that
      case _ =>
        false
    }
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[JsObject]

  override def hashCode(): Int = MurmurHash3.unorderedHash(underlying, MurmurHash3.setSeed)
}

object JsObject extends (Seq[(String, JsValue)] => JsObject) {

  /**
   * INTERNAL API: create a fields map by wrapping a Java LinkedHashMap.
   *
   * We use this because the Java implementation better handles hash code collisions for Comparable keys.
   */
  private[json] def createFieldsMap(fields: Iterable[(String, JsValue)] = Seq.empty): immutable.Map[String, JsValue] = {
    (ImmutableLinkedHashMap.newBuilder ++= fields).result()
  }

  /**
   * Construct a new JsObject, with the order of fields in the Seq.
   */
  def apply(fields: collection.Seq[(String, JsValue)]): JsObject = new JsObject(createFieldsMap(fields))

  /** An empty JSON object */
  def empty = JsObject(Seq.empty)

  /** Identity writes */
  implicit def writes: OWrites[JsObject] = OWrites[JsObject](identity)
}
