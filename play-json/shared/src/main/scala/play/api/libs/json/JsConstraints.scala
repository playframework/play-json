/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.collection.immutable.Map

trait ConstraintFormat {
  @inline def of[A](implicit fmt: Format[A]): Format[A] = fmt

  def optionWithNull[A](implicit fmt: Format[A]): Format[Option[A]] =
    Format[Option[A]](Reads.optionWithNull(fmt), Writes.optionWithNull(fmt))
}

trait PathFormat {
  def at[A](path: JsPath)(implicit f: Format[A]): OFormat[A] =
    OFormat[A](Reads.at(path)(f), Writes.at(path)(f))

  def withDefault[A](path: JsPath, defaultValue: => A)(implicit f: Format[A]): OFormat[A] =
    OFormat[A](Reads.withDefault(path, defaultValue)(f), Writes.at(path)(f))

  def nullable[A](path: JsPath)(implicit f: Format[A]): OFormat[Option[A]] =
    OFormat(Reads.nullable(path)(f), Writes.nullable(path)(f))

  def nullableWithDefault[A](path: JsPath, defaultValue: => Option[A])(implicit f: Format[A]): OFormat[Option[A]] =
    OFormat(Reads.nullableWithDefault(path, defaultValue)(f), Writes.nullable(path)(f))
}

trait PathReads {
  def required(path: JsPath)(implicit reads: Reads[JsValue]): Reads[JsValue] = at(path)(reads)

  def at[A](path: JsPath)(implicit reads: Reads[A]): Reads[A] =
    Reads[A](js => path.asSingleJsResult(js).flatMap(reads.reads(_).repath(path)))

  def withDefault[A](path: JsPath, defaultValue: => A)(implicit reads: Reads[A]): Reads[A] =
    nullable[A](path).map(_.getOrElse(defaultValue))

  /**
   * Reads a Option[T] search optional or nullable field at JsPath (field not found or null is None
   * and other cases are Error).
   *
   * It runs through JsValue following all JsPath nodes on JsValue:
   * - If any node in JsPath is not found => returns None
   * - If any node in JsPath is found with value "null" => returns None
   * - If the entire path is found => applies implicit Reads[T]
   */
  def nullable[A](path: JsPath)(implicit reads: Reads[A]): Reads[Option[A]] =
    nullableWithDefault(path, None)

  /**
   * Reads a Option[T] search nullable field at JsPath (null is None
   * and other cases are Error).
   *
   * It runs through JsValue following all JsPath nodes on JsValue:
   * - If any node in JsPath is not found => returns default value
   * - If the last node in JsPath is found with value "null" => returns None
   * - If the entire path is found => applies implicit Reads[T]
   */
  def nullableWithDefault[A](path: JsPath, defaultValue: => Option[A])(implicit reads: Reads[A]): Reads[Option[A]] =
    Reads[Option[A]] { json =>
      path.asSingleJson(json) match {
        case JsDefined(JsNull) => JsSuccess(None)
        case JsDefined(value)  => reads.reads(value).repath(path).map(Some(_))
        case _: JsUndefined    => JsSuccess(defaultValue)
      }
    }

  def jsPick[A <: JsValue](path: JsPath)(implicit reads: Reads[A]): Reads[A] = at(path)(reads)

  def jsPickBranch[A <: JsValue](path: JsPath)(implicit reads: Reads[A]): Reads[JsObject] =
    Reads[JsObject](js =>
      path
        .asSingleJsResult(js)
        .flatMap { jsv =>
          reads.reads(jsv).repath(path)
        }
        .map(jsv => JsPath.createObj(path -> jsv))
    )

  def jsPut(path: JsPath, a: => JsValue) = Reads[JsObject](json => JsSuccess(JsPath.createObj(path -> a)))

  def jsCopyTo[A <: JsValue](path: JsPath)(reads: Reads[A]) =
    Reads[JsObject](js => reads.reads(js).map(js => JsPath.createObj(path -> js)))

  def jsUpdate[A <: JsValue](path: JsPath)(reads: Reads[A]) =
    Reads[JsObject] {
      case o: JsObject =>
        path
          .asSingleJsResult(o)
          .flatMap(js => reads.reads(js).repath(path))
          .map(jsv => JsPath.createObj(path -> jsv))
          .map(opath => o.deepMerge(opath))
      case _ =>
        JsError(JsPath, JsonValidationError("error.expected.jsobject"))
    }

  def jsPrune(path: JsPath) = Reads[JsObject](js => path.prune(js))
}

trait ConstraintReads {

  /** The simpler of all Reads that just finds an implicit Reads[A] of the expected type */
  @inline def of[A](implicit r: Reads[A]) = r

  /** very simple optional field Reads that maps "null" to None */
  def optionWithNull[T](implicit rds: Reads[T]): Reads[Option[T]] = Reads {
    case JsNull => JsSuccess(None)
    case js     => rds.reads(js).map(Some(_))
  }

  /** Stupidly reads a field as an Option mapping any error (format or missing field) to None */
  def optionNoError[A](implicit reads: Reads[A]): Reads[Option[A]] =
    Reads[Option[A]](js => JsSuccess(reads.reads(js).asOpt))

  def list[A](implicit reads: Reads[A]): Reads[List[A]]                            = Reads.traversableReads[List, A]
  def set[A](implicit reads: Reads[A]): Reads[Set[A]]                              = Reads.traversableReads[Set, A]
  def seq[A](implicit reads: Reads[A]): Reads[Seq[A]]                              = Reads.traversableReads[Seq, A]
  def map[A](implicit reads: Reads[A]): Reads[collection.immutable.Map[String, A]] = Reads.mapReads[A]

  /**
   * Defines a minimum value for a Reads. Combine with `max` using `andKeep`, e.g.
   * `.read(Reads.min(0) andKeep Reads.max(100))`.
   */
  def min[O](m: O)(implicit reads: Reads[O], ord: Ordering[O]) =
    filterNot[O](JsonValidationError("error.min", m))(ord.lt(_, m))(reads)

  /**
   * Defines a maximum value for a Reads. Combine with `min` using `andKeep`, e.g.
   * `.read(Reads.min(0.1) andKeep Reads.max(1.0))`.
   */
  def max[O](m: O)(implicit reads: Reads[O], ord: Ordering[O]) =
    filterNot[O](JsonValidationError("error.max", m))(ord.gt(_, m))(reads)

  def filterNot[A](error: JsonValidationError)(p: A => Boolean)(implicit reads: Reads[A]) =
    Reads[A](js => reads.reads(js).filterNot(JsError(error))(p))

  def filter[A](otherwise: JsonValidationError)(p: A => Boolean)(implicit reads: Reads[A]) =
    Reads[A](js => reads.reads(js).filter(JsError(otherwise))(p))

  def minLength[M](m: Int)(implicit reads: Reads[M], p: M => scala.collection.Iterable[_]) =
    filterNot[M](JsonValidationError("error.minLength", m))(p(_).size < m)

  def maxLength[M](m: Int)(implicit reads: Reads[M], p: M => scala.collection.Iterable[_]) =
    filterNot[M](JsonValidationError("error.maxLength", m))(p(_).size > m)

  /**
   * Defines a regular expression constraint for `String` values, i.e. the string must match the regular expression pattern
   */
  def pattern(regex: => scala.util.matching.Regex, error: String = "error.pattern")(implicit reads: Reads[String]) =
    Reads[String](js =>
      reads.reads(js).flatMap { o =>
        regex.unapplySeq(o).map(_ => JsSuccess(o)).getOrElse(JsError(error))
      }
    )

  def email(implicit reads: Reads[String]): Reads[String] =
    pattern(
      """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r,
      "error.email"
    )

  def verifying[A](cond: A => Boolean)(implicit rds: Reads[A]) =
    filter[A](JsonValidationError("error.invalid"))(cond)(rds)

  def verifyingIf[A](cond: A => Boolean)(subreads: Reads[_])(implicit rds: Reads[A]) =
    Reads[A] { js =>
      rds.reads(js).flatMap { t =>
        scala.util.control.Exception
          .catching(classOf[MatchError])
          .opt(cond(t))
          .flatMap { b =>
            if (b) Some(subreads.reads(js).map(_ => t))
            else None
          }
          .getOrElse(JsSuccess(t))
      }
    }

  def pure[A](a: => A) = Reads[A] { js =>
    JsSuccess(a)
  }
}

trait PathWrites {
  def at[A](path: JsPath)(implicit wrs: Writes[A]): OWrites[A] =
    OWrites[A] { a =>
      JsPath.createObj(path -> wrs.writes(a))
    }

  /**
   * writes a optional field in given JsPath : if None, doesn't write field at all.
   * Please note we do not write "null" but simply omit the field when None
   * If you want to write a "null", use ConstraintWrites.optionWithNull[A]
   */
  def nullable[A](path: JsPath)(implicit wrs: Writes[A]): OWrites[Option[A]] =
    OWrites[Option[A]] {
      case Some(a) => JsPath.createObj(path -> wrs.writes(a))
      case None    => JsObject.empty
    }

  def jsPick(path: JsPath): Writes[JsValue] =
    Writes[JsValue] { obj =>
      path(obj).headOption.getOrElse(JsNull)
    }

  def jsPickBranch(path: JsPath): OWrites[JsValue] =
    OWrites[JsValue] { obj =>
      JsPath.createObj(path -> path(obj).headOption.getOrElse(JsNull))
    }

  def jsPickBranchUpdate(path: JsPath, wrs: OWrites[JsValue]): OWrites[JsValue] =
    OWrites[JsValue] { js =>
      JsPath.createObj(
        path -> path(js).headOption
          .flatMap(js => js.asOpt[JsObject].map(obj => obj.deepMerge(wrs.writes(obj))))
          .getOrElse(JsNull)
      )
    }

  def pure[A](path: JsPath, fixed: => A)(implicit wrs: Writes[A]): OWrites[JsValue] =
    OWrites[JsValue] { js =>
      JsPath.createObj(path -> wrs.writes(fixed))
    }
}

trait ConstraintWrites {
  def of[A](implicit w: Writes[A]) = w

  def pure[A](fixed: => A)(implicit wrs: Writes[A]): Writes[JsValue] =
    Writes[JsValue] { js =>
      wrs.writes(fixed)
    }

  @deprecated("Use `pruned` without `Writes[A]`", "2.8.0")
  def pruned[A](implicit w: Writes[A]): Writes[A] = new Writes[A] {
    def writes(a: A): JsValue = JsNull
  }

  def pruned[A]: Writes[A] = new Writes[A] {
    def writes(a: A): JsValue = JsNull
  }

  def list[A](implicit writes: Writes[A]): Writes[List[A]] = Writes.iterableWrites2[A, List[A]]

  def set[A](implicit writes: Writes[A]): Writes[Set[A]] = Writes.iterableWrites2[A, Set[A]]

  def seq[A](implicit writes: Writes[A]): Writes[Seq[A]] = Writes.iterableWrites2[A, Seq[A]]

  def map[A](implicit writes: Writes[A]): OWrites[Map[String, A]] = Writes.genericMapWrites[A, Map]

  /**
   * Pure Option Writer[T] which writes "null" when None which is different
   * from `JsPath.writeNullable` which omits the field when None
   */
  def optionWithNull[A](implicit wa: Writes[A]) = Writes[Option[A]] {
    case Some(av) => wa.writes(av)
    case _        => JsNull
  }
}
