/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json
import scala.annotation.implicitNotFound
import play.api.libs.functional._

/**
 * Json formatter: write an implicit to define both a serializer and a deserializer for any type.
 */
@implicitNotFound("No Json formatter found for type ${A}. Try to implement an implicit Format for this type.")
trait Format[A] extends Writes[A] with Reads[A] {

  /**
   * Maps reads and writes operations between the types `A` and `B`,
   * using the given functions.
   *
   * @param readsMap the function applied to the read `A` value
   * @param writesContraMap the function to produce a `A` from `B`before writing
   */
  def bimap[B](readsMap: A => B, writesContramap: B => A): Format[B] =
    Format[B](map(readsMap), contramap(writesContramap).writes(_))
}

trait OFormat[A] extends OWrites[A] with Reads[A] with Format[A] {
  final override def bimap[B](readsMap: A => B, writesContramap: B => A): OFormat[B] =
    OFormat.invariantFunctorOFormat.inmap(this, readsMap, writesContramap)

}

object OFormat extends ScalaCompatOFormat {
  implicit def functionalCanBuildFormats(implicit
      rcb: FunctionalCanBuild[Reads],
      wcb: FunctionalCanBuild[OWrites]
  ): FunctionalCanBuild[OFormat] = new FunctionalCanBuild[OFormat] {
    def apply[A, B](fa: OFormat[A], fb: OFormat[B]): OFormat[A ~ B] =
      OFormat[A ~ B](rcb(fa, fb), wcb(fa, fb))
  }

  implicit val invariantFunctorOFormat: InvariantFunctor[OFormat] = new InvariantFunctor[OFormat] {
    def inmap[A, B](fa: OFormat[A], f1: A => B, f2: B => A): OFormat[B] =
      OFormat[B]((js: JsValue) => fa.reads(js).map(f1), (b: B) => fa.writes(f2(b)))
  }

  @deprecated("Use `oFormatFromReadsAndOWrites`", "2.7.0")
  def GenericOFormat[T](implicit fjs: Reads[T], tjs: OWrites[T]): Format[T] = apply(fjs, tjs)

  implicit def oFormatFromReadsAndOWrites[T](implicit fjs: Reads[T], tjs: OWrites[T]): OFormat[T] = apply(fjs, tjs)

  def apply[A](read: JsValue => JsResult[A], write: A => JsObject): OFormat[A] = new OFormat[A] {
    def reads(js: JsValue): JsResult[A] = read(js)

    def writes(a: A): JsObject = write(a)
  }

  def apply[A](r: Reads[A], w: OWrites[A]): OFormat[A] = new OFormat[A] {
    def reads(js: JsValue): JsResult[A] = r.reads(js)

    def writes(a: A): JsObject = w.writes(a)
  }
}

/**
 * Default Json formatters.
 */
object Format extends PathFormat with ConstraintFormat with DefaultFormat with ScalaCompatFormat {
  val constraints: ConstraintFormat = this
  val path: PathFormat              = this

  implicit val invariantFunctorFormat: InvariantFunctor[Format] =
    new InvariantFunctor[Format] {
      def inmap[A, B](fa: Format[A], f1: A => B, f2: B => A) =
        Format(fa.map(f1), Writes(b => fa.writes(f2(b))))
    }

  def apply[A](fjs: Reads[A], tjs: Writes[A]): Format[A] = new Format[A] {
    def reads(json: JsValue) = fjs.reads(json)
    def writes(o: A)         = tjs.writes(o)
  }

  /**
   * Evidence that values of type `T` are represented as values of type `R`.
   *
   * This type carries no runtime information and is only used to provide
   * compile-time evidence of the representation.
   *
   * @tparam T the represented type
   * @tparam R the representation type
   */
  @implicitNotFound(
    "No JSON representation found for ${T} as ${R}. An implicit Format.Representation[${T}, ${R}] is required."
  )
  trait Representation[T, R <: JsValue]

  object Representation extends EnvFormatRepresentation {
    private object Unsafe extends Representation[Nothing, JsValue] {}

    def asString[T]: Representation[T, JsString] =
      Unsafe.asInstanceOf[Representation[T, JsString]]

    implicit val uriReadsRepresentation: Representation[java.net.URI, JsString] =
      asString[java.net.URI]

    implicit val uuidReadsRepresentation: Representation[java.util.UUID, JsString] =
      asString[java.util.UUID]

    /**
     * Provides [[Representation]] instances for types that may be represented
     * as JSON strings depending on the [[Writes]] instance in scope.
     *
     * These instances are not available implicitly by default and must be
     * explicitly imported when a string representation is required, for example
     * when using such types as JSON object keys.
     */
    object Implicits extends EnvRepresentations
  }
}

/**
 * Default Json formatters.
 */
trait DefaultFormat {
  implicit def GenericFormat[T](implicit fjs: Reads[T], tjs: Writes[T]): Format[T] = new Format[T] {
    def reads(json: JsValue) = fjs.reads(json)
    def writes(o: T)         = tjs.writes(o)
  }
}
