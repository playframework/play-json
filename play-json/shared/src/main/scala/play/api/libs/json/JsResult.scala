/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.collection.Seq

/**
 * The result for a successful parsing.
 */
case class JsSuccess[T](value: T, path: JsPath = JsPath()) extends JsResult[T] {
  def get: T = value

  val isSuccess = true
  val isError   = false

  def fold[U](invalid: Seq[(JsPath, Seq[JsonValidationError])] => U, valid: T => U): U = valid(value)

  def map[U](f: T => U): JsResult[U] = copy(value = f(value))

  def flatMap[U](f: T => JsResult[U]): JsResult[U] = f(value).repath(path)

  def foreach(f: T => Unit): Unit = f(value)

  def contains[AA >: T](elem: AA): Boolean = elem == value

  def exists(p: T => Boolean): Boolean = p(value)

  def forall(p: T => Boolean): Boolean = p(value)

  def repath(path: JsPath): JsResult[T] = JsSuccess(value, path ++ this.path)

  def getOrElse[U >: T](t: => U): U = value

  def orElse[U >: T](t: => JsResult[U]): JsResult[U] = this

  def asOpt: Option[T] = Some(value)

  def asEither: Either[Seq[(JsPath, Seq[JsonValidationError])], T] = Right(value)

  def recover[U >: T](errManager: PartialFunction[JsError, U]): JsResult[U] = this

  def recoverTotal[U >: T](errManager: JsError => U): U = value

  def recoverWith[U >: T](errManager: JsError => JsResult[U]): JsResult[U] = this
}

/**
 * The result in case of parsing `errors`.
 */
case class JsError(errors: Seq[(JsPath, Seq[JsonValidationError])]) extends JsResult[Nothing] {
  def get: Nothing = throw new NoSuchElementException("JsError.get")

  def ++(error: JsError): JsError = JsError.merge(this, error)

  def :+(error: (JsPath, JsonValidationError)): JsError     = JsError.merge(this, JsError(error))
  def append(error: (JsPath, JsonValidationError)): JsError = this.:+(error)

  def +:(error: (JsPath, JsonValidationError)): JsError      = JsError.merge(JsError(error), this)
  def prepend(error: (JsPath, JsonValidationError)): JsError = this.+:(error)

  val isSuccess = false
  val isError   = true

  def fold[U](invalid: Seq[(JsPath, Seq[JsonValidationError])] => U, valid: Nothing => U): U = invalid(errors)

  def map[U](f: Nothing => U): JsResult[U] = this

  def flatMap[U](f: Nothing => JsResult[U]): JsResult[U] = this

  def foreach(f: Nothing => Unit): Unit = ()

  def contains[AA >: Nothing](elem: AA): Boolean = false

  def exists(p: Nothing => Boolean): Boolean = false

  def forall(p: Nothing => Boolean): Boolean = true

  def repath(path: JsPath): JsResult[Nothing] =
    JsError(JsResult.repath(errors, path))

  def getOrElse[U >: Nothing](t: => U): U = t

  def orElse[U >: Nothing](t: => JsResult[U]): JsResult[U] = t

  val asOpt = None

  def asEither: Either[Seq[(JsPath, Seq[JsonValidationError])], Nothing] = Left(errors)

  def recover[U >: Nothing](errManager: PartialFunction[JsError, U]): JsResult[U] = JsSuccess(errManager(this))

  def recoverTotal[U >: Nothing](errManager: JsError => U): U = errManager(this)

  def recoverWith[U >: Nothing](errManager: JsError => JsResult[U]): JsResult[U] = errManager(this)
}

object JsError {
  def apply(): JsError = JsError(Seq(JsPath -> Seq()))

  def apply(error: JsonValidationError): JsError = JsError(Seq(JsPath -> Seq(error)))

  def apply(error: String): JsError = JsError(JsonValidationError(error))

  def apply(error: (JsPath, JsonValidationError)): JsError = JsError(Seq(error._1 -> Seq(error._2)))

  def apply(path: JsPath, error: JsonValidationError): JsError = JsError(path -> error)

  def apply(path: JsPath, error: String): JsError = JsError(path -> JsonValidationError(error))

  def merge(
      e1: Seq[(JsPath, Seq[JsonValidationError])],
      e2: Seq[(JsPath, Seq[JsonValidationError])]
  ): Seq[(JsPath, Seq[JsonValidationError])] = {
    (e1 ++ e2).groupBy(_._1).iterator.map { case (k, v) => k -> v.flatMap(_._2) }.toList
  }

  def merge(e1: JsError, e2: JsError): JsError = {
    JsError(merge(e1.errors, e2.errors))
  }

  def toJson(e: JsError): JsObject = toJson(e.errors, false)

  def toJson(errors: Seq[(JsPath, Seq[JsonValidationError])]): JsObject = toJson(errors, false)

  // def toJsonErrorsOnly: JsValue = original // TODO
  def toFlatForm(e: JsError): Seq[(String, Seq[JsonValidationError])] = e.errors.map { case (path, seq) =>
    path.toJsonString -> seq
  }

  private def toJson(errors: Seq[(JsPath, Seq[JsonValidationError])], flat: Boolean): JsObject = {
    errors.foldLeft(JsObject.empty) { (obj, error) =>
      obj ++ JsObject(Seq(error._1.toJsonString -> error._2.foldLeft(JsArray.empty) { (arr, err) =>
        val msg = JsArray(Predef.wrapRefArray[JsValue] {
          if (flat) Array(JsString(err.message))
          else err.messages.map(JsString(_)).toArray[JsValue]
        })
        arr :+ JsObject(
          Seq(
            "msg"  -> msg,
            "args" -> JsArray(err.args.map(toJson).toArray[JsValue])
          )
        )
      }))
    }
  }

  /**
   * Serializer for Any, used for the args of errors.
   */
  private def toJson(a: Any): JsValue = a match {
    case s: String   => JsString(s)
    case nb: Int     => JsNumber(nb)
    case nb: Short   => JsNumber(BigDecimal(nb))
    case nb: Long    => JsNumber(BigDecimal(nb))
    case nb: Double  => JsNumber(BigDecimal(nb))
    case nb: Float   => JsNumber(BigDecimal.decimal(nb))
    case b: Boolean  => JsBoolean(b)
    case js: JsValue => js
    case x           => JsString(x.toString)
  }

  /**
   * Extracts the first error message.
   *
   * {{{
   * import play.api.libs.json.JsError
   *
   * def msg(err: JsError): Option[String] = err match {
   *   case JsError.Message(msg) => Some(msg)
   *   case _ => None
   * }
   * }}}
   */
  object Message {

    def unapply(error: JsError): Option[String] =
      error.errors.headOption.collect { case (_, JsonValidationError.Message(msg) +: _) =>
        msg
      }
  }

  /**
   * Extracts the first error details (message and its first argument).
   *
   * {{{
   * import play.api.libs.json.JsError
   *
   * def cause(err: JsError): Option[(String, Exception)] = err match {
   *   case JsError.Detailed(msg, ex: Exception) => Some(msg -> ex)
   *   case _ => None
   * }
   * }}}
   */
  object Detailed {

    def unapply(error: JsError): Option[(String, Any)] =
      error.errors.headOption.collect { case (_, JsonValidationError.Detailed(msg, arg) +: _) =>
        msg -> arg
      }
  }
}

sealed trait JsResult[+A] { self =>
  def isSuccess: Boolean
  def isError: Boolean

  /**
   * Either applies the `invalid` function if this result is an error,
   * or applies the `valid` function on the successful value.
   */
  def fold[B](invalid: Seq[(JsPath, Seq[JsonValidationError])] => B, valid: A => B): B

  /**
   * If this result is successful, applies the function `f` on the parsed value.
   */
  def map[B](f: A => B): JsResult[B]

  /**
   * If this result is successful, applies the function `f` on the parsed value.
   */
  def flatMap[B](f: A => JsResult[B]): JsResult[B]

  /**
   * If this result is successful, applies the function `f` on the parsed value.
   */
  def foreach(f: A => Unit): Unit

  def filterNot(error: JsError)(p: A => Boolean): JsResult[A] =
    flatMap { a => if (p(a)) error else JsSuccess(a) }

  def filterNot(p: A => Boolean): JsResult[A] =
    flatMap { a => if (p(a)) JsError() else JsSuccess(a) }

  def filter(p: A => Boolean): JsResult[A] =
    flatMap { a => if (p(a)) JsSuccess(a) else JsError() }

  def filter(otherwise: JsError)(p: A => Boolean): JsResult[A] =
    flatMap { a => if (p(a)) JsSuccess(a) else otherwise }

  def collect[B](otherwise: JsonValidationError)(p: PartialFunction[A, B]): JsResult[B] = flatMap {
    case t if p.isDefinedAt(t) => JsSuccess(p(t))
    case _                     => JsError(otherwise)
  }

  def withFilter(p: A => Boolean) = new WithFilter(p)

  final class WithFilter(p: A => Boolean) {

    def map[B](f: A => B): JsResult[B] = self match {
      case JsSuccess(a, path) =>
        if (p(a)) JsSuccess(f(a), path)
        else JsError()
      case e @ JsError(_) => e
    }

    def flatMap[B](f: A => JsResult[B]): JsResult[B] = self match {
      case JsSuccess(a, path) =>
        if (p(a)) f(a).repath(path)
        else JsError()
      case e @ JsError(_) => e
    }

    def foreach(f: A => Unit): Unit = self match {
      case JsSuccess(a, _) if p(a) => f(a)
      case _                       => ()
    }

    def withFilter(q: A => Boolean) = new WithFilter(a => p(a) && q(a))
  }

  /** If this result is successful than checks for presence for '''elem''', otherwise return '''false''' */
  def contains[AA >: A](elem: AA): Boolean

  /** If this result is successful than check value with predicate '''p''', otherwise return '''false''' */
  def exists(p: A => Boolean): Boolean

  /**
   * If this result is successful than check value with predicate '''p''', otherwise return '''true'''.
   * Follows [[scala.collection.Traversable.forall]] semantics
   */
  def forall(p: A => Boolean): Boolean

  /** Updates the JSON path */
  def repath(path: JsPath): JsResult[A]

  /** Not recommended */
  def get: A

  /** Either returns the successful value, or the value from `t`. */
  def getOrElse[AA >: A](t: => AA): AA

  /**
   * Either returns this result if successful, or the result from `t`.
   */
  def orElse[AA >: A](t: => JsResult[AA]): JsResult[AA]

  /**
   * Transforms this result either `Some` option with the successful value,
   * or as `None` in case of JSON error.
   */
  def asOpt: Option[A]

  /**
   * Returns either the result errors (at `Left`),
   * or the successful value (at `Right`).
   */
  def asEither: Either[Seq[(JsPath, Seq[JsonValidationError])], A]

  /**
   * If this result is not successful,
   * recovers the errors with the given function.
   */
  def recover[AA >: A](errManager: PartialFunction[JsError, AA]): JsResult[AA]

  /**
   * If this result is not successful,
   * recovers the errors with the given function.
   */
  def recoverTotal[AA >: A](errManager: JsError => AA): AA

  /**
   * If this result is not successful,
   * recovers the errors with the given function.
   */
  def recoverWith[AA >: A](errManager: JsError => JsResult[AA]): JsResult[AA]
}

object JsResult {
  import scala.util.Failure
  import scala.util.Try
  import scala.util.Success
  import play.api.libs.functional._

  case class Exception(cause: JsError)
      extends java.lang.Exception(Json.stringify(JsError.toJson(cause)))
      with scala.util.control.NoStackTrace

  /**
   * Returns a JSON validation as a [[scala.util.Try]].
   *
   * @tparam T the type for the parsing
   * @param result the JSON validation result
   * @param err the function to be applied if the results is an error
   *
   * {{{
   * import scala.concurrent.Future
   * import play.api.libs.json.JsResult
   *
   * def toFuture[T](res: JsResult[T]): Future[T] =
   *   Future.fromTry(JsResult.toTry(res))
   * }}}
   */
  def toTry[T](result: JsResult[T], err: JsError => Throwable = Exception(_)): Try[T] = result match {
    case e @ JsError(_)      => Failure(err(e))
    case s @ JsSuccess(v, _) => Success(v)
  }

  /**
   * Returns a [[scala.util.Try]] as JSON validation.
   *
   * @tparam T the type for the parsing
   * @param result the result
   * @param err the function to be applied for [[scala.util.Failure]]
   */
  def fromTry[T](
      result: Try[T],
      err: Throwable => JsError = { e => JsError(e.getMessage) }
  ): JsResult[T] = result match {
    case Success(v) => JsSuccess(v)
    case Failure(e) => err(e)
  }

  implicit def alternativeJsResult(implicit
      a: Applicative[JsResult]
  ): Alternative[JsResult] =
    new Alternative[JsResult] {
      val app = a

      def |[A, B >: A](alt1: JsResult[A], alt2: JsResult[B]): JsResult[B] = (alt1, alt2) match {
        case (JsError(e), JsSuccess(t, p)) => JsSuccess(t, p)
        case (JsSuccess(t, p), _)          => JsSuccess(t, p)
        case (JsError(e1), JsError(e2))    => JsError(JsError.merge(e1, e2))
      }
      def empty: JsResult[Nothing] = JsError(Seq.empty)
    }

  implicit val applicativeJsResult: Applicative[JsResult] = new Applicative[JsResult] {
    def pure[A](f: => A): JsResult[A] = JsSuccess(f)

    def map[A, B](m: JsResult[A], f: A => B): JsResult[B] = m.map(f)

    def apply[A, B](mf: JsResult[A => B], ma: JsResult[A]): JsResult[B] = (mf, ma) match {
      case (JsSuccess(f, _), JsSuccess(a, _)) => JsSuccess(f(a))
      case (JsError(e1), JsError(e2))         => JsError(JsError.merge(e1, e2))
      case (JsError(e), _)                    => JsError(e)
      case (_, JsError(e))                    => JsError(e)
    }
  }

  implicit val functorJsResult: Functor[JsResult] = new Functor[JsResult] {
    override def fmap[A, B](m: JsResult[A], f: A => B) = m.map(f)
  }

  private[JsResult] type Errors = Seq[(JsPath, Seq[JsonValidationError])]

  private[json] def repath(errors: Errors, path: JsPath): Errors =
    errors.map { case (p, s) => (path ++ p) -> s }
}
