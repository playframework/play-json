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

object JsValue extends JsValueCompat {
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
sealed abstract class JsNumber extends JsValue with Serializable {
  def value: BigDecimal

  /** Canonical JSON representation */
  private[json] def text: String

  override def hashCode: Int = text.hashCode

  override def toString: String = s"JsNumber($text)"
}

private[json] sealed trait JsNumberLowPriority { self: JsNumber.type =>
  def apply(value: BigInt): JsNumber = new JsBigInteger(value)

  // ---

  private[json] final class JsBigInteger(private[json] val underlying: BigInt) extends JsNumber {
    lazy val value: BigDecimal = BigDecimal(underlying)

    lazy val text: String = {
      if (underlying.isValidInt) {
        Numbers.formatInt(underlying.toInt)
      } else if (underlying.isValidLong) {
        Numbers.formatLong(underlying.toLong)
      } else {
        underlying.toString
      }
    }

    @annotation.nowarn("msg=.*outer\\ reference.*")
    override def equals(that: Any): Boolean = that match {
      case other: JsBigInteger =>
        this.underlying == other.underlying

      case other: JsNumber.JsLong => {
        if (underlying.isValidLong) other.underlying == underlying.toLong
        else false
      }

      case other: JsLazy if other.numberType == NumberType.Integer =>
        this.value == other.value

      case JsNumber(otherValue) =>
        this.value == otherValue

      case _ =>
        false
    }
  }
}

object JsNumber
    extends scala.runtime.AbstractFunction1[BigDecimal, JsNumber]
    with JsNumberLowPriority
    with JsNumberExtractors
    with JsNumberCompat {

  def apply(value: BigDecimal): JsNumber = new JsBigDecimal(value)

  def apply(i: Int): JsNumber = new JsNumericInt(i, implicitly[Numeric[Int]])

  def apply(s: Short): JsNumber = new JsNumericInt(s, implicitly[Numeric[Short]])

  def apply(b: Byte): JsNumber = new JsNumericInt(b, implicitly[Numeric[Byte]])

  def apply(l: Long): JsNumber = new JsLong(l)

  def apply(f: Float): JsNumber =
    new JsNumericDouble(f.toDouble, BigDecimal(f), f)

  def apply(d: Double): JsNumber =
    new JsNumericDouble(d, BigDecimal(d), d.toFloat)

  // Extractors

  def unapply(value: JsValue): Option[BigDecimal] = value match {
    case n: JsNumber => Some(n.value)
    case _           => None
  }

  // ---

  private[libs] final class JsLong(private[json] val underlying: Long) extends JsNumber {
    def value = BigDecimal(underlying)

    // Canonical JSON string representation
    private[json] lazy val text = Numbers.formatLong(underlying)

    override def equals(that: Any): Boolean = that match {
      case ValidLong(other) =>
        other == underlying

      case _ =>
        false
    }
  }

  /**
   * @tparam T any numeric integer type (`Short`, `Int`) different from `Long` or `BigInt`
   */
  private[libs] final class JsNumericInt[T](
      private[json] val underlying: T,
      private[json] val numeric: Numeric[T]
  ) extends JsNumber {
    lazy val value = BigDecimal(toLong)

    lazy val text = toInt.toString

    private[json] lazy val toInt = numeric.toInt(underlying)

    private[json] def toLong = numeric.toLong(underlying)

    private[json] def toDouble = numeric.toDouble(underlying)

    override def equals(that: Any): Boolean = that match {
      case other: JsNumericInt[?] =>
        this.underlying == other.underlying

      case other: JsNumericDouble =>
        toDouble == other.underlying

      case other: JsLong =>
        numeric.toLong(underlying) == other.underlying

      case other: JsLazy if other.numberType == NumberType.Integer =>
        other.value.toInt == toInt

      case other: JsLazy =>
        other.value.toDouble == toDouble // TODO: Range check

      case JsNumber(`value`) =>
        true

      case _ =>
        false
    }
  }

  /**
   * @tparam T any numeric integer type up to `Double` precision different from `BigDecimal`
   */
  private[libs] final class JsNumericDouble(
      private[json] val underlying: Double,
      repr: => BigDecimal,
      float: => Float
  ) extends JsNumber {
    lazy val value = repr

    private[json] lazy val toFloat = float

    lazy val text = {
      if (underlying.isWhole) {
        Numbers.formatLong(underlying.toLong)
      } else {
        Numbers.formatDouble(underlying)
      }
    }

    override def equals(that: Any): Boolean = that match {
      case other: JsNumericInt[?] =>
        this.underlying == other.underlying

      case other: JsNumericDouble =>
        underlying == other.underlying

      case other: JsLong => {
        val d = underlying

        d.isWhole && d >= Long.MinValue && d <= Long.MaxValue &&
        d.toLong == other.underlying
      }

      case other: JsLazy =>
        underlying == other.value.toDouble // TODO: Range check

      case JsNumber(otherValue) =>
        this.value == otherValue

      case _ =>
        false
    }
  }

  private[libs] final class JsBigDecimal(val value: BigDecimal) extends JsNumber {
    lazy val text: String = {
      if (value.isWhole) {
        if (value.isValidInt) {
          Numbers.formatInt(value.toInt)
        } else if (value.isValidLong) {
          Numbers.formatLong(value.toLong)
        } else {
          value.toString
        }
      } else {
        val d = value.toDouble

        if (!d.isInfinite && BigDecimal(d) == value) {
          Numbers.formatDouble(d)
        } else {
          value.toString
        }
      }
    }

    override def equals(that: Any): Boolean = that match {
      case other: JsBigDecimal =>
        this.value == other.value

      case JsNumber(otherValue) =>
        this.value == otherValue

      case _ =>
        false
    }
  }

  /**
   * @param text the JSON canonical number representation
   */
  private[libs] final class JsLazy(
      val numberType: NumberType,
      private[json] val text: String,
      bigDecimal: => BigDecimal
  ) extends JsNumber {
    lazy val value = bigDecimal

    @annotation.nowarn("msg=.*outer\\ reference.*")
    override def equals(that: Any): Boolean = that match {
      case other: JsLazy =>
        this.text == other.text && this.numberType == other.numberType

      case other: JsLong if numberType == NumberType.Integer =>
        Numbers.isValidLong(text) &&
        Numbers.parseLong(text) == other.underlying

      case other: JsNumericInt[?] if numberType == NumberType.Integer =>
        value.toInt == other.toInt // TODO: Range check

      case other: JsNumericDouble if numberType == NumberType.Float =>
        value.toDouble == other.underlying // TODO: Range check

      case other: JsBigInteger if numberType == NumberType.Integer =>
        this.value == other.value

      case JsNumber(otherValue) =>
        this.bigDecimal == otherValue

      case _ =>
        false
    }
  }

  // ---

  private[json] sealed abstract class NumberType {
    def name: String

    final override def toString = name
  }

  private[json] object NumberType {
    case object Integer extends NumberType {
      def name = "Integer"
    }

    case object Float extends NumberType {
      def name = "Float"
    }
  }
}

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
  @annotation.nowarn("cat=deprecation&msg=.*Map.*")
  def -(otherField: String): JsObject = JsObject(underlying - otherField)

  /**
   * Adds one field to the JsObject
   */
  @annotation.nowarn("cat=deprecation&msg=.*Map.*")
  def +(otherField: (String, JsValue)): JsObject = JsObject(underlying + otherField)

  /**
   * Merges everything in depth and doesn't stop at first level, as ++ does
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
