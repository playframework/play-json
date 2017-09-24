/*
 * Copyright (C) 2009-2017 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import play.api.libs.functional.ContravariantFunctor

import scala.annotation.implicitNotFound
import scala.collection._
import scala.reflect.ClassTag

/**
 * Json serializer: write an implicit to define a serializer for any type
 */
@implicitNotFound(
  "No Json serializer found for type ${A}. Try to implement an implicit Writes or Format for this type."
)
trait Writes[-A] {
  /**
   * Convert the object into a JsValue
   */
  def writes(o: A): JsValue

  /**
   * Transforms the resulting [[JsValue]] using transformer function.
   */
  def transform(transformer: JsValue => JsValue): Writes[A] = Writes[A] { a => transformer(this.writes(a)) }

  /**
   * Transforms the resulting [[JsValue]] using a `Writes[JsValue]`.
   */
  def transform(transformer: Writes[JsValue]): Writes[A] = Writes[A] { a => transformer.writes(this.writes(a)) }
}

@implicitNotFound(
  "No Json serializer as JsObject found for type ${A}. Try to implement an implicit OWrites or OFormat for this type."
)
trait OWrites[-A] extends Writes[A] {
  def writes(o: A): JsObject

  /**
   * Transforms the resulting [[JsObject]] using a transformer function.
   */
  def transform(transformer: JsObject => JsObject): OWrites[A] =
    OWrites[A] { a => transformer(this.writes(a)) }

  /**
   * Transforms the resulting [[JsValue]] using a `Writes[JsValue]`.
   */
  def transform(transformer: OWrites[JsObject]): OWrites[A] =
    OWrites[A] { a => transformer.writes(this.writes(a)) }

}

object OWrites extends PathWrites with ConstraintWrites {
  import play.api.libs.functional._

  /**
   * An `OWrites` merging the results of two separate `OWrites`.
   */
  private object MergedOWrites {
    def apply[A, B](wa: OWrites[A], wb: OWrites[B]): OWrites[A ~ B] =
      new OWritesFromFields[A ~ B] {
        def writeFields(fieldsMap: mutable.Map[String, JsValue], obj: A ~ B): Unit = {
          val a ~ b = obj
          mergeIn(fieldsMap, wa, a)
          mergeIn(fieldsMap, wb, b)
        }
      }

    @inline final def mergeIn[A](fieldsMap: mutable.Map[String, JsValue], wa: Writes[A], a: A): Unit = wa match {
      case wff: OWritesFromFields[A] =>
        wff.writeFields(fieldsMap, a)
      case w: OWrites[A] =>
        w.writes(a).underlying.foreach {
          case (key, value: JsObject) =>
            fieldsMap.put(key, fieldsMap.get(key) match {
              case Some(o: JsObject) => o deepMerge value
              case _ => value
            })
          case (key, value) =>
            fieldsMap.put(key, value)
        }
    }
  }

  /**
   * An `OWrites` capable of writing an object incrementally to a mutable map
   */
  private trait OWritesFromFields[A] extends OWrites[A] {
    def writeFields(fieldsMap: mutable.Map[String, JsValue], a: A)

    def writes(a: A): JsObject = {
      val fieldsMap = new mutable.LinkedHashMap[String, JsValue]()
      writeFields(fieldsMap, a)
      JsObject(fieldsMap)
    }
  }

  implicit val functionalCanBuildOWrites: FunctionalCanBuild[OWrites] = new FunctionalCanBuild[OWrites] {
    def apply[A, B](wa: OWrites[A], wb: OWrites[B]): OWrites[A ~ B] = MergedOWrites[A, B](wa, wb)
  }

  implicit val contravariantfunctorOWrites: ContravariantFunctor[OWrites] = new ContravariantFunctor[OWrites] {

    def contramap[A, B](wa: OWrites[A], f: B => A): OWrites[B] = OWrites[B](b => wa.writes(f(b)))

  }

  def apply[A](f: A => JsObject): OWrites[A] = new OWrites[A] {
    def writes(a: A): JsObject = f(a)
  }

  /**
   * Transforms the resulting [[JsObject]] using the given function,
   * which is also applied with the initial input.
   * def transform(transformer: (A, JsObject) => JsObject): OWrites[A] =
   * OWrites[A] { a => transformer(a, this.writes(a)) }
   *
   * @param w the initial writer
   * @param f the transformer function
   */
  def transform[A](w: OWrites[A])(f: (A, JsObject) => JsObject): OWrites[A] =
    OWrites[A] { a => f(a, w.writes(a)) }
}

/**
 * Default Serializers.
 */
object Writes extends PathWrites with ConstraintWrites with DefaultWrites with GeneratedWrites {

  val constraints: ConstraintWrites = this
  val path: PathWrites = this

  implicit val contravariantfunctorWrites: ContravariantFunctor[Writes] =
    new ContravariantFunctor[Writes] {
      def contramap[A, B](wa: Writes[A], f: B => A): Writes[B] =
        Writes[B](b => wa.writes(f(b)))
    }

  def apply[A](f: A => JsValue): Writes[A] = new Writes[A] {
    def writes(a: A): JsValue = f(a)
  }

  /**
   * Transforms the resulting [[JsValue]] using the given function,
   * which is also applied with the initial input.
   * def transform(transformer: (A, JsValue) => JsValue): Writes[A] =
   * Writes[A] { a => transformer(a, this.writes(a)) }
   *
   * @param w the initial writer
   * @param f the transformer function
   */
  def transform[A](w: Writes[A])(f: (A, JsValue) => JsValue): Writes[A] =
    Writes[A] { a => f(a, w.writes(a)) }
}

/**
 * Default Serializers.
 */
trait DefaultWrites extends LowPriorityWrites {
  import scala.language.implicitConversions

  /**
   * Serializer for Int types.
   */
  implicit object IntWrites extends Writes[Int] {
    def writes(o: Int) = JsNumber(o)
  }

  /**
   * Serializer for Short types.
   */
  implicit object ShortWrites extends Writes[Short] {
    def writes(o: Short) = JsNumber(o)
  }

  /**
   * Serializer for Byte types.
   */
  implicit object ByteWrites extends Writes[Byte] {
    def writes(o: Byte) = JsNumber(o)
  }

  /**
   * Serializer for Long types.
   */
  implicit object LongWrites extends Writes[Long] {
    def writes(o: Long) = JsNumber(o)
  }

  /**
   * Serializer for Float types.
   */
  implicit object FloatWrites extends Writes[Float] {
    def writes(o: Float) = JsNumber(o)
  }

  /**
   * Serializer for Double types.
   */
  implicit object DoubleWrites extends Writes[Double] {
    def writes(o: Double) = JsNumber(o)
  }

  /**
   * Serializer for BigDecimal types.
   */
  implicit object BigDecimalWrites extends Writes[BigDecimal] {
    def writes(o: BigDecimal) = JsNumber(o)
  }

  /**
   * Serializer for Boolean types.
   */
  implicit object BooleanWrites extends Writes[Boolean] {
    def writes(o: Boolean) = JsBoolean(o)
  }

  /**
   * Serializer for String types.
   */
  implicit object StringWrites extends Writes[String] {
    def writes(o: String) = JsString(o)
  }

  /**
   * Serializer for Array[T] types.
   */
  implicit def arrayWrites[T: ClassTag: Writes]: Writes[Array[T]] = {
    val w = implicitly[Writes[T]]

    Writes[Array[T]] { ts => JsArray(ts.map(w.writes(_)).toArray[JsValue]) }
  }

  /**
   * Serializer for Map[String,V] types.
   */
  implicit def mapWrites[V: Writes]: OWrites[Map[String, V]] = {
    val w = implicitly[Writes[V]]

    OWrites[Map[String, V]] { ts => JsObject(ts.mapValues(w.writes(_)).toSeq) }
  }

  /**
   * Serializer for JsValues.
   */
  implicit object JsValueWrites extends Writes[JsValue] {
    def writes(o: JsValue) = o
  }

  /**
   * Serializer for Option.
   */
  implicit def OptionWrites[T](implicit fmt: Writes[T]): Writes[Option[T]] = new Writes[Option[T]] {
    def writes(o: Option[T]) = o match {
      case Some(value) => fmt.writes(value)
      case None => JsNull
    }
  }

  /**
   * Serializer for java.util.Date
   * @param pattern the pattern used by SimpleDateFormat
   */
  def dateWrites(pattern: String): Writes[java.util.Date] = new Writes[java.util.Date] {
    def writes(d: java.util.Date): JsValue = JsString(new java.text.SimpleDateFormat(pattern).format(d))
  }

  /**
   * Default Serializer java.util.Date -> JsNumber(d.getTime (nb of ms))
   */
  implicit object DefaultDateWrites extends Writes[java.util.Date] {
    def writes(d: java.util.Date): JsValue = JsNumber(d.getTime)
  }

  /**
   * Serializer for java.sql.Date
   * @param pattern the pattern used by SimpleDateFormat
   */
  def sqlDateWrites(pattern: String): Writes[java.sql.Date] = new Writes[java.sql.Date] {
    def writes(d: java.sql.Date): JsValue = JsString(new java.text.SimpleDateFormat(pattern).format(d))
  }

  /**
   * Serializer for java.util.UUID
   */
  implicit object UuidWrites extends Writes[java.util.UUID] {
    def writes(u: java.util.UUID) = JsString(u.toString)
  }

  /**
   * Serializer for scala.Enumeration by name.
   */
  implicit def enumNameWrites[E <: Enumeration]: Writes[E#Value] =
    Writes[E#Value] { value: E#Value => JsString(value.toString) }

}

sealed trait LowPriorityWrites extends EnvWrites {
  /**
   * Serializer for Traversables types.
   */
  implicit def traversableWrites[A: Writes]: Writes[Traversable[A]] = {
    val w = implicitly[Writes[A]]

    Writes[Traversable[A]] { as =>
      val builder = mutable.ArrayBuilder.make[JsValue]()
      as.foreach { a =>
        builder += w.writes(a)
      }
      JsArray(builder.result())
    }
    // Avoid resolution ambiguity with more specific Traversable Writes,
    // such as OWrites.map
  }
}
