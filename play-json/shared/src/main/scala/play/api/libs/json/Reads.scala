/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import java.net.URI

import scala.annotation.implicitNotFound

import scala.util.Try

import scala.collection.Seq
import scala.collection.immutable.Map
import scala.collection.mutable.Builder

import scala.reflect.ClassTag

import ScalaCollectionCompat._

/**
 * A `Reads` object describes how to decode JSON into a value.
 * `Reads` objects are typically provided as implicit values. When `Reads`
 * implicit values are in scope, a program is able to deserialize JSON
 * into values of the right type.
 *
 * The inverse of a `Reads` object is a [[Writes]] object, which describes
 * how to encode a value into JSON. If you combine a `Reads` and a `Writes`
 * then you get a [[Format]].
 */
@implicitNotFound(
  "No Json deserializer found for type ${A}. Try to implement an implicit Reads or Format for this type."
)
trait Reads[A] { self =>

  /**
   * Convert the JsValue into a A
   */
  def reads(json: JsValue): JsResult[A]

  /**
   * Create a new `Reads` which maps the value produced by this `Reads`.
   *
   * @tparam B The type of the value produced by the new `Reads`.
   * @param f the function applied on the result of the current instance,
   * if successful
   */
  def map[B](f: A => B): Reads[B] = Reads[B] { self.reads(_).map(f) }

  def flatMap[B](f: A => Reads[B]): Reads[B] = Reads[B] { json =>
    // Do not flatMap result to avoid repath
    self.reads(json) match {
      case JsSuccess(a, _)    => f(a).reads(json)
      case error @ JsError(_) => error
    }
  }

  def filter(f: A => Boolean): Reads[A] = Reads[A] { self.reads(_).filter(f) }

  def filter(error: JsonValidationError)(f: A => Boolean): Reads[A] =
    Reads[A] { json =>
      self.reads(json).filter(JsError(error))(f)
    }

  def filterNot(f: A => Boolean): Reads[A] =
    Reads[A] { json =>
      self.reads(json).filterNot(f)
    }

  def filterNot(error: JsonValidationError)(f: A => Boolean): Reads[A] =
    Reads[A] { json =>
      self.reads(json).filterNot(JsError(error))(f)
    }

  def collect[B](error: JsonValidationError)(f: PartialFunction[A, B]): Reads[B] =
    Reads[B] { json =>
      self.reads(json).collect(error)(f)
    }

  /**
   * Creates a new `Reads`, based on this one, which first executes this
   * `Reads`' logic then, if this `Reads` resulted in a `JsError`, runs
   * the second `Reads` on the [[JsValue]].
   *
   * @param v the `Reads` to run if this one gets a `JsError`
   * @return A new `Reads` with the updated behavior.
   */
  def orElse(v: Reads[A]): Reads[A] =
    Reads[A] { json =>
      self.reads(json).orElse(v.reads(json))
    }

  @deprecated("Use [[composeWith]]", "2.7.0")
  def compose[B <: JsValue](rb: Reads[B]): Reads[A] = composeWith[B](rb)

  /**
   * Creates a new `Reads`, which first passes the input JSON to `rb`,
   * and then it executes this `Reads` on the pre-processed JSON
   * (if `rb` has successfully handled the input JSON).
   */
  def composeWith[B <: JsValue](rb: Reads[B]): Reads[A] = Reads[A] {
    rb.reads(_) match {
      case JsSuccess(b, p) => this.reads(b).repath(p)
      case JsError(e)      => JsError(e)
    }
  }

  /**
   * Creates a new `Reads`, which first transforms the input JSON
   * using the given `tranformer`, and then it executes this `Reads`
   * on the pre-processed JSON.
   *
   * @param transformer the function to pre-process the input JSON
   */
  def preprocess(f: PartialFunction[JsValue, JsValue]): Reads[A] =
    Reads[A] { input =>
      val json = f.applyOrElse(input, identity[JsValue])

      this.reads(json)
    }

  /**
   * Creates a new `Reads`, which transforms the successful result
   * from the current instance using the given function.
   *
   * @param f the function applied on the successful `A` value
   *
   * {{{
   * final class Foo private(val code: String) extends AnyVal
   *
   * val A = new Foo("A")
   * val B = new Foo("B")
   *
   * import play.api.libs.json.Reads
   *
   * val r: Reads[Foo] = implicitly[Reads[String]].flatMapResult {
   *   case "A" => JsSuccess(A)
   *   case "B" => JsSuccess(B)
   *   case _   => JsError("error.expected.foo")
   * }
   * }}}
   */
  def flatMapResult[B](f: A => JsResult[B]): Reads[B] = Reads[B] {
    this.reads(_).flatMap(f)
  }

  def andThen[B](rb: Reads[B])(implicit witness: A <:< JsValue): Reads[B] =
    rb.composeWith(this.map(witness))

  /**
   * Widen this `Reads`.
   *
   * {{{
   * import play.api.libs.json.Reads
   *
   * sealed trait Animal
   * case class Dog(name: String) extends Animal
   * case class Cat(name: String) extends Animal
   *
   * def simple(r: Reads[Dog]): Reads[Animal] = r.widen[Animal]
   * }}}
   */
  def widen[B >: A]: Reads[B] = Reads[B] { self.reads(_) }
}

/**
 * Default deserializer type classes.
 */
object Reads extends ConstraintReads with PathReads with DefaultReads with GeneratedReads {
  val constraints: ConstraintReads = this

  val path: PathReads = this

  /**
   * Returns a `JsSuccess(a)` (with root path) for any JSON value read.
   *
   * {{{
   * import play.api.libs.json.Reads
   *
   * val r: Reads[String] = Reads.pure("foo")
   * }}}
   *
   * @see [[failed]]
   */
  override def pure[A](f: => A): Reads[A] = Reads[A] { _ =>
    JsSuccess(f)
  }

  /**
   * Returns a `JsError(cause)` for any JSON value read.
   *
   * {{{
   * import play.api.libs.json.Reads
   *
   * val r: Reads[String] = Reads.failed[String]("Failure message")
   * }}}
   *
   * @see [[pure]]
   */
  def failed[A](msg: => String): Reads[A] = Reads[A] { _ =>
    JsError(msg)
  }

  @deprecated("Use `pure` with `f:=>A` parameter", "2.7.0")
  private[json] def pure[A](value: A): Reads[A] =
    Reads[A] { _ =>
      JsSuccess(value)
    }

  import play.api.libs.functional._

  implicit def applicative(implicit applicativeJsResult: Applicative[JsResult]): Applicative[Reads] =
    new Applicative[Reads] {
      def pure[A](f: => A): Reads[A] = Reads.pure(f = f)

      def map[A, B](m: Reads[A], f: A => B): Reads[B] = m.map(f)

      def apply[A, B](mf: Reads[A => B], ma: Reads[A]): Reads[B] = new Reads[B] {
        def reads(js: JsValue) = applicativeJsResult(mf.reads(js), ma.reads(js))
      }
    }

  implicit def alternative(implicit a: Applicative[Reads]): Alternative[Reads] = new Alternative[Reads] {
    val app = a
    def |[A, B >: A](alt1: Reads[A], alt2: Reads[B]): Reads[B] = new Reads[B] {
      def reads(js: JsValue) = alt1.reads(js) match {
        case r @ JsSuccess(_, _) => r
        case JsError(es1) =>
          alt2.reads(js) match {
            case r2 @ JsSuccess(_, _) => r2
            case JsError(es2)         => JsError(JsError.merge(es1, es2))
          }
      }
    }

    def empty: Reads[Nothing] =
      new Reads[Nothing] { def reads(js: JsValue) = JsError(Seq()) }
  }

  /**
   * Returns an instance which uses `f` as [[Reads.reads]] function.
   */
  def apply[A](f: JsValue => JsResult[A]): Reads[A] =
    new Reads[A] { def reads(json: JsValue) = f(json) }

  implicit def functorReads(implicit a: Applicative[Reads]): Functor[Reads] = new Functor[Reads] {
    def fmap[A, B](reads: Reads[A], f: A => B): Reads[B] = a.map(reads, f)
  }

  implicit object JsObjectMonoid extends Monoid[JsObject] {
    def append(o1: JsObject, o2: JsObject) = o1.deepMerge(o2)
    def identity                           = JsObject(Seq.empty)
  }

  implicit val JsObjectReducer: Reducer[JsObject, JsObject] = Reducer[JsObject, JsObject](o => o)

  implicit object JsArrayMonoid extends Monoid[JsArray] {
    def append(a1: JsArray, a2: JsArray) = a1 ++ a2
    def identity                         = JsArray()
  }

  implicit val JsArrayReducer: Reducer[JsValue, JsArray] = Reducer[JsValue, JsArray](js => JsArray(Array(js)))
}

/**
 * Low priority reads.
 *
 * This exists as a compiler performance optimization, so that the compiler doesn't have to rule them out when
 * DefaultReads provides a simple match.
 *
 * See https://github.com/playframework/playframework/issues/4313 for more details.
 */
trait LowPriorityDefaultReads extends EnvReads {
  implicit def keyMapReads[K: KeyReads, V](implicit fmtv: Reads[V]): Reads[Map[K, V]] =
    Reads.mapReads[K, V](implicitly[KeyReads[K]].readKey _)

  /**
   * Generic deserializer for collections types.
   */
  implicit def traversableReads[F[_], A](implicit bf: Factory[A, F[A]], ra: Reads[A]): Reads[F[A]] = new Reads[F[A]] {
    def reads(json: JsValue) = json match {
      case JsArray(ts) =>
        ts.iterator.zipWithIndex
          .foldLeft(JsSuccess {
            val b = bf.newBuilder
            b.sizeHint(ts)
            b
          }: JsResult[Builder[A, F[A]]]) {
            case (acc, (elem, idx)) =>
              (acc, ra.reads(elem)) match {
                case (JsSuccess(vs, _), JsSuccess(v, _)) => JsSuccess(vs += v)
                case (_: JsSuccess[_], jsError: JsError) => jsError.repath(JsPath(idx))
                case (JsError(errors0), JsError(errors)) => JsError(errors0 ++ JsResult.repath(errors, JsPath(idx)))
                case (jsError: JsError, _: JsSuccess[_]) => jsError
              }
          }
          .map(_.result())
      case _ => JsError(Seq(JsPath -> Seq(JsonValidationError("error.expected.jsarray"))))
    }
  }
}

/**
 * Default deserializer type classes.
 */
trait DefaultReads extends LowPriorityDefaultReads {
  import scala.language.implicitConversions

  /**
   * builds a JsErrorObj JsObject
   * {
   *    __VAL__ : "current known erroneous jsvalue",
   *    __ERR__ : "the i18n key of the error msg",
   *    __ARGS__ : "the args for the error msg" (JsArray)
   * }
   */
  def JsErrorObj(knownValue: JsValue, key: String, args: JsValue*): JsObject = {
    JsObject(
      Seq(
        "__VAL__"  -> knownValue,
        "__ERR__"  -> JsString(key),
        "__ARGS__" -> args.foldLeft(JsArray())((acc: JsArray, arg: JsValue) => acc :+ arg)
      )
    )
  }

  /**
   * Deserializer for Int types.
   */
  implicit object IntReads extends Reads[Int] {
    def reads(json: JsValue) = json match {
      case JsNumber(n) if n.isValidInt => JsSuccess(n.toInt)
      case JsNumber(n)                 => JsError("error.expected.int")
      case _                           => JsError("error.expected.jsnumber")
    }
  }

  /**
   * Deserializer for Short types.
   */
  implicit object ShortReads extends Reads[Short] {
    def reads(json: JsValue) = json match {
      case JsNumber(n) if n.isValidShort => JsSuccess(n.toShort)
      case JsNumber(n)                   => JsError("error.expected.short")
      case _                             => JsError("error.expected.jsnumber")
    }
  }

  /**
   * Deserializer for Byte types.
   */
  implicit object ByteReads extends Reads[Byte] {
    def reads(json: JsValue) = json match {
      case JsNumber(n) if n.isValidByte => JsSuccess(n.toByte)
      case JsNumber(n)                  => JsError("error.expected.byte")
      case _                            => JsError("error.expected.jsnumber")
    }
  }

  /**
   * Deserializer for Long types.
   */
  implicit object LongReads extends Reads[Long] {
    def reads(json: JsValue) = json match {
      case JsNumber(n) if n.isValidLong => JsSuccess(n.toLong)
      case JsNumber(n)                  => JsError("error.expected.long")
      case _                            => JsError("error.expected.jsnumber")
    }
  }

  /**
   * Deserializer for Float types.
   */
  implicit object FloatReads extends Reads[Float] {
    def reads(json: JsValue) = json match {
      case JsNumber(n) => JsSuccess(n.toFloat)
      case _           => JsError("error.expected.jsnumber")
    }
  }

  /**
   * Deserializer for Double types.
   */
  implicit object DoubleReads extends Reads[Double] {
    def reads(json: JsValue) = json match {
      case JsNumber(n) => JsSuccess(n.toDouble)
      case _           => JsError("error.expected.jsnumber")
    }
  }

  /**
   * Deserializer for BigDecimal
   */
  implicit val javaBigDecReads: Reads[java.math.BigDecimal] = Reads[java.math.BigDecimal](js =>
    js match {
      case JsString(s) => parseBigDecimal(s)
      case JsNumber(d) => JsSuccess(d.underlying)
      case _           => JsError(JsonValidationError("error.expected.jsnumberorjsstring"))
    }
  )

  /**
   * Deserializer for BigDecimal
   */
  implicit val bigDecReads: Reads[BigDecimal] = javaBigDecReads.map(BigDecimal(_))

  /**
   * Deserializer for BigInteger
   */
  implicit object BigIntegerReads extends Reads[java.math.BigInteger] {
    def reads(json: JsValue) = json match {
      case JsString(s) =>
        parseBigInteger(s)

      case JsNumber(d) =>
        d.toBigIntExact match {
          case Some(i) => JsSuccess(i.underlying)
          case _       => JsError(JsonValidationError("error.invalid.biginteger"))
        }

      case _ =>
        JsError(JsonValidationError("error.expected.jsnumberorjsstring"))
    }
  }

  /**
   * Deserializer for BigInt
   */
  implicit object BigIntReads extends Reads[BigInt] {
    def reads(json: JsValue) = BigIntegerReads.reads(json).map(BigInt(_))
  }

  /**
   * Reads for `scala.Enumeration` types using the name.
   *
   * @param enum a `scala.Enumeration`.
   */
  def enumNameReads[E <: Enumeration](`enum`: E): Reads[`enum`.Value] = new Reads[`enum`.Value] {
    def reads(json: JsValue) = json match {
      case JsString(str) =>
        `enum`.values
          .find(_.toString == str)
          .map(JsSuccess(_))
          .getOrElse(JsError(Seq(JsPath -> Seq(JsonValidationError("error.expected.validenumvalue")))))
      case _ => JsError(Seq(JsPath -> Seq(JsonValidationError("error.expected.enumstring"))))
    }
  }

  /**
   * Deserializer for Boolean types.
   */
  implicit object BooleanReads extends Reads[Boolean] {
    def reads(json: JsValue) = json match {
      case JsBoolean(b) => JsSuccess(b)
      case _            => JsError(Seq(JsPath -> Seq(JsonValidationError("error.expected.jsboolean"))))
    }
  }

  /**
   * Deserializer for String types.
   */
  implicit object StringReads extends Reads[String] {
    def reads(json: JsValue) = json match {
      case JsString(s) => JsSuccess(s)
      case _           => JsError(Seq(JsPath -> Seq(JsonValidationError("error.expected.jsstring"))))
    }
  }

  /**
   * Deserializer for JsObject.
   */
  implicit object JsObjectReads extends Reads[JsObject] {
    def reads(json: JsValue) = json match {
      case o: JsObject => JsSuccess(o)
      case _           => JsError(Seq(JsPath -> Seq(JsonValidationError("error.expected.jsobject"))))
    }
  }

  /**
   * Deserializer for JsArray.
   */
  implicit object JsArrayReads extends Reads[JsArray] {
    def reads(json: JsValue) = json match {
      case o: JsArray => JsSuccess(o)
      case _          => JsError(Seq(JsPath -> Seq(JsonValidationError("error.expected.jsarray"))))
    }
  }

  /**
   * Deserializer for JsValue.
   */
  implicit object JsValueReads extends Reads[JsValue] {
    def reads(json: JsValue) = JsSuccess(json)
  }

  /**
   * Deserializer for JsString.
   */
  implicit object JsStringReads extends Reads[JsString] {
    def reads(json: JsValue) = json match {
      case s: JsString => JsSuccess(s)
      case _           => JsError(Seq(JsPath -> Seq(JsonValidationError("error.expected.jsstring"))))
    }
  }

  /**
   * Deserializer for JsNumber.
   */
  implicit object JsNumberReads extends Reads[JsNumber] {
    def reads(json: JsValue) = json match {
      case n: JsNumber => JsSuccess(n)
      case _           => JsError(Seq(JsPath -> Seq(JsonValidationError("error.expected.jsnumber"))))
    }
  }

  /**
   * Deserializer for JsBoolean.
   */
  implicit object JsBooleanReads extends Reads[JsBoolean] {
    def reads(json: JsValue) = json match {
      case b: JsBoolean => JsSuccess(b)
      case _            => JsError(Seq(JsPath -> Seq(JsonValidationError("error.expected.jsboolean"))))
    }
  }

  @scala.annotation.tailrec
  private def mapObj[K, V](
      key: String => JsResult[K],
      in: List[(String, JsValue)],
      out: Builder[(K, V), Map[K, V]]
  )(implicit vr: Reads[V]): JsResult[Map[K, V]] = in match {
    case (k, v) :: entries =>
      key(k).flatMap(vk => v.validate[V].map(vk -> _)) match {
        case JsError(details) => JsError(details)

        case JsSuccess((vk, value), _) =>
          mapObj[K, V](key, entries, out += (vk -> value))
      }

    case _ => JsSuccess(out.result())
  }

  /** Deserializer for a `Map[K,V]` */
  implicit def mapReads[K, V](k: String => JsResult[K])(implicit fmtv: Reads[V]): Reads[Map[K, V]] = Reads[Map[K, V]] {
    case JsObject(m) => {
      type Errors = Seq[(JsPath, Seq[JsonValidationError])]
      def locate(e: Errors, key: String) = e.map {
        case (p, valerr) =>
          (JsPath \ key) ++ p -> valerr
      }

      // !! Keep accumulating the error after the first one
      m.foldLeft(Right(Map.empty): Either[Errors, Map[K, V]]) {
        case (acc, (key, value)) =>
          val result = for {
            rv <- fmtv.reads(value)
            rk <- k(key)
          } yield rk -> rv

          (acc, result) match {
            case (Right(vs), JsSuccess(v, _)) => Right(vs + v)
            case (Right(_), JsError(e))       => Left(locate(e, key))
            case (Left(e), _: JsSuccess[_])   => Left(e)
            case (Left(e1), JsError(e2))      => Left(e1 ++ locate(e2, key))
          }
      }.fold(JsError.apply, res => JsSuccess(res))
    }

    case _ => JsError("error.expected.jsobject")
  }

  /** Deserializer for a `Map[String,V]` */
  implicit def mapReads[V](implicit fmtv: Reads[V]): Reads[Map[String, V]] =
    mapReads[String, V](JsSuccess(_))

  /** Deserializer for a `Map[Char, V]` */
  def charMapReads[V](implicit vr: Reads[V]): Reads[Map[Char, V]] =
    mapReads[Char, V] { str =>
      if (str.size == 1) JsSuccess(str.charAt(0))
      else JsError("error.invalid.character")
    }

  /**
   * Deserializer for Array[T] types.
   */
  implicit def ArrayReads[T: Reads: ClassTag]: Reads[Array[T]] = new Reads[Array[T]] {
    def reads(json: JsValue) = json.validate[List[T]].map(_.toArray)
  }

  /**
   * Deserializer for java.net.URI
   */
  implicit val uriReads: Reads[URI] = Reads[URI] {
    _.validate[String].flatMap { repr =>
      JsResult.fromTry(Try(new URI(repr)))
    }
  }

  /**
   * Deserializer for java.util.UUID
   */
  class UUIDReader(checkValidity: Boolean) extends Reads[java.util.UUID] {
    import java.util.UUID

    import scala.util.Try

    def check(s: String)(u: UUID): Boolean = u != null && s == u.toString()
    def parseUuid(s: String): Option[UUID] = {
      val uncheckedUuid = Try(UUID.fromString(s)).toOption

      if (checkValidity) {
        uncheckedUuid.filter(check(s))
      } else {
        uncheckedUuid
      }
    }

    def reads(json: JsValue) = json match {
      case JsString(s) => {
        parseUuid(s)
          .map(JsSuccess(_))
          .getOrElse(JsError(Seq(JsPath -> Seq(JsonValidationError("error.expected.uuid")))))
      }
      case _ => JsError(Seq(JsPath -> Seq(JsonValidationError("error.expected.uuid"))))
    }
  }

  implicit val uuidReads: Reads[java.util.UUID] = new UUIDReader(false)
}
