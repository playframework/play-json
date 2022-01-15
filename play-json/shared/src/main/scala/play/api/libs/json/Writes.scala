/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import java.util.Date

import scala.annotation.implicitNotFound

import scala.collection._
import scala.reflect.ClassTag

import play.api.libs.functional.ContravariantFunctor

/**
 * Json serializer: write an implicit to define a serializer for any type
 */
@implicitNotFound(
  "No Json serializer found for type ${A}. Try to implement an implicit Writes or Format for this type."
)
trait Writes[A] { self =>

  /**
   * Converts the `A` value into a [[JsValue]].
   */
  def writes(o: A): JsValue

  /**
   * Returns a new instance that first converts a `B` value to a `A` one,
   * before converting this `A` value into a [[JsValue]].
   */
  def contramap[B](f: B => A): Writes[B] = Writes[B](b => self.writes(f(b)))

  /**
   * Narrows to any `B` super-type of `A`.
   */
  def narrow[B <: A]: Writes[B] = this.asInstanceOf[Writes[B]]

  /**
   * Transforms the resulting [[JsValue]] using transformer function.
   */
  def transform(transformer: JsValue => JsValue): Writes[A] = Writes[A] { a =>
    transformer(self.writes(a))
  }

  /**
   * Transforms the resulting [[JsValue]] using a `Writes[JsValue]`.
   */
  def transform(transformer: Writes[JsValue]): Writes[A] = Writes[A] { a =>
    transformer.writes(self.writes(a))
  }
}

@implicitNotFound(
  "No Json serializer as JsObject found for type ${A}. Try to implement an implicit OWrites or OFormat for this type."
)
trait OWrites[A] extends Writes[A] {
  def writes(o: A): JsObject

  /**
   * Transforms the resulting [[JsObject]] using a transformer function.
   */
  def transform(transformer: JsObject => JsObject): OWrites[A] =
    OWrites[A] { a =>
      transformer(this.writes(a))
    }

  /**
   * Transforms the resulting [[JsValue]] using a `Writes[JsValue]`.
   */
  def transform(transformer: OWrites[JsObject]): OWrites[A] =
    OWrites[A] { a =>
      transformer.writes(this.writes(a))
    }

  override def contramap[B](f: B => A): OWrites[B] =
    OWrites[B](b => this.writes(f(b)))

  override def narrow[B <: A]: OWrites[B] = this.asInstanceOf[OWrites[B]]
}

object OWrites extends PathWrites with ConstraintWrites {
  import play.api.libs.functional._

  def of[A](implicit w: OWrites[A]): OWrites[A] = w

  def pure[A](fixed: => A)(implicit wrs: OWrites[A]): OWrites[JsValue] =
    OWrites[JsValue] { js =>
      wrs.writes(fixed)
    }

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

    @inline final def mergeIn[A](fieldsMap: mutable.Map[String, JsValue], wa: OWrites[A], a: A): Unit = wa match {
      case wff: OWritesFromFields[A] =>
        wff.writeFields(fieldsMap, a)
      case w: OWrites[A] =>
        w.writes(a).underlying.foreach {
          case (key, value: JsObject) =>
            fieldsMap.put(
              key,
              fieldsMap.get(key) match {
                case Some(o: JsObject) => o.deepMerge(value)
                case _                 => value
              }
            )
          case (key, value) =>
            fieldsMap.put(key, value)
        }
    }
  }

  /**
   * An `OWrites` capable of writing an object incrementally to a mutable map
   */
  private trait OWritesFromFields[A] extends OWrites[A] {
    def writeFields(fieldsMap: mutable.Map[String, JsValue], a: A): Unit

    def writes(a: A): JsObject = {
      val fieldsMap = JsObject.createFieldsMap()
      writeFields(fieldsMap, a)
      JsObject(fieldsMap)
    }
  }

  implicit val functionalCanBuildOWrites: FunctionalCanBuild[OWrites] = new FunctionalCanBuild[OWrites] {
    def apply[A, B](wa: OWrites[A], wb: OWrites[B]): OWrites[A ~ B] = MergedOWrites[A, B](wa, wb)
  }

  implicit val contravariantfunctorOWrites: ContravariantFunctor[OWrites] = new ContravariantFunctor[OWrites] {
    def contramap[A, B](wa: OWrites[A], f: B => A): OWrites[B] =
      wa.contramap[B](f)
  }

  /**
   * Returns an instance which uses `f` as [[OWrites.writes]] function.
   */
  def apply[A](f: A => JsObject): OWrites[A] = new OWrites[A] {
    def writes(a: A): JsObject = f(a)
  }

  /**
   * Transforms the resulting [[JsObject]] using the given function,
   * which is also applied with the initial input.
   *
   * @param w the initial writer
   * @param f the transformer function
   */
  def transform[A](w: OWrites[A])(f: (A, JsObject) => JsObject): OWrites[A] =
    OWrites[A] { a =>
      f(a, w.writes(a))
    }
}

/**
 * Default Serializers.
 */
object Writes extends PathWrites with ConstraintWrites with DefaultWrites with GeneratedWrites {
  val constraints: ConstraintWrites = this
  val path: PathWrites              = this

  implicit val contravariantfunctorWrites: ContravariantFunctor[Writes] =
    new ContravariantFunctor[Writes] {
      def contramap[A, B](wa: Writes[A], f: B => A): Writes[B] =
        wa.contramap[B](f)
    }

  /**
   * Returns an instance which uses `f` as [[Writes.writes]] function.
   */
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
    Writes[A] { a =>
      f(a, w.writes(a))
    }
}

/**
 * Default Serializers.
 */
trait DefaultWrites extends LowPriorityWrites with EnumerationWrites {

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
    def writes(o: Short) = JsNumber(BigDecimal(o))
  }

  /**
   * Serializer for Byte types.
   */
  implicit object ByteWrites extends Writes[Byte] {
    def writes(o: Byte) = JsNumber(BigDecimal(o))
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
    def writes(o: Float) = JsNumber(BigDecimal.decimal(o))
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
   * Serializer for BigInt type.
   */
  implicit object BigIntWrites extends Writes[BigInt] {
    def writes(i: BigInt) = JsNumber(BigDecimal(i))
  }

  /**
   * Serializer for BigInteger type.
   */
  implicit object BigIntegerWrites extends Writes[java.math.BigInteger] {
    def writes(i: java.math.BigInteger) = JsNumber(BigDecimal(i))
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

    Writes[Array[T]] { ts =>
      JsArray(ts.map(w.writes(_)).toArray[JsValue])
    }
  }

  /**
   * Serializer for Map[String,V] types.
   */
  @deprecated("Use `genericMapWrites`", "2.8.0")
  implicit def mapWrites[V: Writes]: OWrites[MapWrites.Map[String, V]] = MapWrites.mapWrites

  implicit def keyMapWrites[K: KeyWrites, V: Writes, M[K, V] <: MapWrites.Map[K, V]]: OWrites[M[K, V]] = {
    val kw = implicitly[KeyWrites[K]]
    val vw = implicitly[Writes[V]]

    OWrites[M[K, V]] { ts =>
      JsObject(ts.toSeq.map { case (k, v) =>
        kw.writeKey(k) -> vw.writes(v)
      })
    }
  }

  /**
   * Serializer for Map[String,V] types.
   */
  implicit def genericMapWrites[V, M[A, B] <: MapWrites.Map[A, B]](implicit w: Writes[V]): OWrites[M[String, V]] =
    OWrites[M[String, V]] { ts =>
      JsObject(ts.iterator.map { case (k, v) => k -> w.writes(v) }.toSeq)
    }

  @deprecated("Use `jsValueWrites`", "2.8.0")
  object JsValueWrites extends Writes[JsValue] {
    def writes(o: JsValue) = o
  }

  /**
   * Serializer for JsValues.
   */
  implicit def jsValueWrites[T <: JsValue]: Writes[T] = Writes[T] { js =>
    js
  }

  /**
   * Serializer for JsNull.
   *
   * {{{
   * import play.api.libs.json.Json
   *
   * Json.obj("foo" -> None)
   * // equivalent to Json.obj("foo" -> JsNull)
   * }}}
   */
  implicit val NoneWrites: Writes[None.type] =
    Writes[None.type] { _ =>
      JsNull
    }

  /**
   * If `Some` is directly used (not as `Option`).
   *
   * {{{
   * import play.api.libs.json.{ Json, Writes }
   *
   * def foo[T: Writes](writeableValue: T) =
   *   Json.obj("foo" -> Some(writeableValue))
   *   // equivalent to Json.obj("foo" -> writeableValue)
   * }}}
   */
  implicit def someWrites[T](implicit w: Writes[T]): Writes[Some[T]] =
    Writes[Some[T]] { some =>
      w.writes(some.get)
    }

  /**
   * Serializer for Option.
   */
  implicit def OptionWrites[T](implicit fmt: Writes[T]): Writes[Option[T]] = new Writes[Option[T]] {
    def writes(o: Option[T]) = o match {
      case Some(value) => fmt.writes(value)
      case None        => JsNull
    }
  }

  /**
   * Serializer for java.util.Date
   * @param pattern the pattern used by SimpleDateFormat
   */
  def dateWrites(pattern: String): Writes[java.util.Date] = new Writes[java.util.Date] {
    def writes(d: java.util.Date): JsValue = JsString(new java.text.SimpleDateFormat(pattern).format(d))
  }

  @deprecated("Use `defaultDateWrites`", "2.8.0")
  object DefaultDateWrites extends Writes[Date] {
    def writes(d: Date): JsValue = JsNumber(d.getTime)
  }

  /**
   * Default Serializer java.util.Date -> JsNumber(d.getTime (nb of ms))
   */
  implicit def defaultDateWrites[T <: Date]: Writes[T] =
    Writes[T] { d =>
      JsNumber(d.getTime)
    }

  /**
   * Serializer for java.sql.Date
   * @param pattern the pattern used by SimpleDateFormat
   */
  @deprecated("Use `dateWrites`", "2.8.0")
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
   * Serializer for [[scala.collection.immutable.Range]]
   * (aka specialized `Seq` of `Int`).
   */
  implicit def rangeWrites[T <: Range]: Writes[T] = Writes[T] { range =>
    // `iterableWrites` cannot be resolved for as,
    // even if `Range <: Traversable[_]`, it's not a parametrized one
    // and so doesn't accept a type parameter (doesn't match `M[_]` constraint).
    JsArray(range.map(JsNumber(_)))
  }
}

sealed trait LowPriorityWrites extends EnvWrites {

  /**
   * Serializer for java.net.URI
   */
  implicit val uriWrites: Writes[java.net.URI] =
    implicitly[Writes[String]].contramap[java.net.URI](_.toString)

  @deprecated("Use `iterableWrites`", "2.8.0")
  def traversableWrites[A: Writes]: Writes[Traversable[A]] = {
    val w = implicitly[Writes[A]]

    Writes[Traversable[A]] { as =>
      val builder = mutable.ArrayBuilder.make[JsValue]
      as.foreach { a =>
        builder += w.writes(a)
      }
      JsArray(builder.result())
    }
    // Avoid resolution ambiguity with more specific Traversable Writes,
    // such as OWrites.map
  }

  /**
   * Serializer for Iterable types.
   *
   * Deprecated due to incompatibility with non `_[_]` shapes, #368.
   */
  @deprecated("Use `iterableWrites2`", "2.8.1")
  def iterableWrites[A, M[T] <: Iterable[T]](implicit w: Writes[A]): Writes[M[A]] =
    iterableWrites2[A, M[A]]

  /**
   * Serializer for Iterable types.
   */
  implicit def iterableWrites2[A, I](implicit ev: I <:< Iterable[A], w: Writes[A]): Writes[I] = {
    // Use Iterable rather than Traversable, for 2.13 compat

    Writes[I] { as =>
      val builder = mutable.ArrayBuilder.make[JsValue]

      as.foreach { (a: A) =>
        builder += w.writes(a)
      }

      JsArray(builder.result())
    }

    // Avoid resolution ambiguity with more specific Iterable Writes,
    // such as OWrites.map
  }

  /**
   * Serializer for any type that is provided an implicit conversion to String
   * (e.g. tagged types).
   */
  implicit def stringableWrites[T](implicit conv: T => String): Writes[T] = Writes.StringWrites.contramap[T](conv)
}
