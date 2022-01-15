/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

sealed trait PathNode {
  def apply(json: JsValue): List[JsValue]
  def toJsonString: String

  private[json] def splitChildren(json: JsValue): List[Either[(PathNode, JsValue), (PathNode, JsValue)]]

  def set(json: JsValue, transform: JsValue => JsValue): JsValue

  private[json] def toJsonField(value: JsValue): JsValue = value
}

case class RecursiveSearch(key: String) extends PathNode {
  def apply(json: JsValue): List[JsValue] = json match {
    case obj: JsObject => (json \\ key).toList
    case arr: JsArray  => (json \\ key).toList
    case _             => Nil
  }
  override def toString = "//" + key
  def toJsonString      = "*" + key

  /**
   * First found, first set and never goes down after setting
   */
  def set(json: JsValue, transform: JsValue => JsValue): JsValue = json match {
    case JsObject(fields) => {
      JsObject(fields.map { case (k, v) =>
        if (k == this.key) {
          k -> transform(v)
        } else k -> set(v, transform)
      })
    }

    case _ => json
  }

  private[json] def splitChildren(json: JsValue) = json match {
    case obj: JsObject =>
      obj.fields.toList.map { case (k, v) =>
        if (k == this.key) Right(this -> v)
        else Left(KeyPathNode(k)      -> v)
      }
    case arr: JsArray =>
      arr.value.toList.zipWithIndex.map { case (js, j) => Left(IdxPathNode(j) -> js) }

    case _ => List()
  }
}

case class KeyPathNode(key: String) extends PathNode {
  def apply(json: JsValue): List[JsValue] = json match {
    case obj: JsObject => obj.underlying.get(key).toList
    case _             => List()
  }

  override def toString = "/" + key
  def toJsonString      = "." + key

  def set(json: JsValue, transform: JsValue => JsValue): JsValue = json match {
    case obj: JsObject =>
      var found = false
      val o = JsObject(obj.fields.map { case (k, v) =>
        if (k == this.key) {
          found = true
          k -> transform(v)
        } else k -> v
      })
      if (!found) o ++ JsObject(Seq(this.key -> transform(JsObject.empty)))
      else o
    case _ => transform(json)
  }

  private[json] def splitChildren(json: JsValue) = json match {
    case obj: JsObject =>
      obj.fields.toList.map { case (k, v) =>
        if (k == this.key) Right(this -> v)
        else Left(KeyPathNode(k)      -> v)
      }
    case _ => List()
  }

  private[json] override def toJsonField(value: JsValue) =
    JsObject(Seq(key -> value))
}

case class IdxPathNode(idx: Int) extends PathNode {
  def apply(json: JsValue): List[JsValue] = json match {
    case arr: JsArray => List(arr \ idx).flatMap(_.toOption)
    case _            => List()
  }

  override def toString = "(%d)".format(idx)
  def toJsonString      = "[%d]".format(idx)

  def set(json: JsValue, transform: JsValue => JsValue): JsValue = json match {
    case arr: JsArray => JsArray(arr.value.zipWithIndex.map { case (js, j) => if (j == idx) transform(js) else js })
    case _            => transform(json)
  }

  private[json] def splitChildren(json: JsValue) = json match {
    case arr: JsArray =>
      arr.value.toList.zipWithIndex.map { case (js, j) =>
        if (j == idx) Right(this -> js)
        else Left(IdxPathNode(j) -> js)
      }
    case _ => List()
  }

  private[json] override def toJsonField(value: JsValue) = value
}

/**
 * Companion object and root path.
 *
 * For an object `{ "name": "foo" }`, the path to the `name` property is:
 *
 * {{{
 * import play.api.libs.json.JsPath
 *
 * JsPath \ "name"
 * }}}
 *
 * For an object `{ "id": 1, "nested": { "score": 0.12 } }`,
 * the path to the nested `score` is:
 *
 * {{{
 * import play.api.libs.json.JsPath
 *
 * JsPath \ "nested" \ "score"
 * }}}
 */
object JsPath extends JsPath(List.empty) {
  // TODO implement it correctly (doesn't merge )
  def createObj(pathValues: (JsPath, JsValue)*): JsObject = {
    def buildSubPath(path: JsPath, value: JsValue) = {
      def step(path: List[PathNode], value: JsValue): JsObject = {
        path match {
          case List() =>
            value match {
              case obj @ JsObject(_) => obj
              case _                 => throw new RuntimeException("when empty JsPath, expecting JsObject")
            }
          case List(p) =>
            p match {
              case KeyPathNode(key) => JsObject(Seq(key -> value))
              case _                => throw new RuntimeException("expected KeyPathNode")
            }
          case head :: tail =>
            head match {
              case KeyPathNode(key) => JsObject(Seq(key -> step(tail, value)))
              case _                => throw new RuntimeException("expected KeyPathNode")
            }
        }
      }

      step(path.path, value)
    }

    // optimize fast path
    val objectMap = JsObject.createFieldsMap()
    val isSimpleObject = pathValues.forall {
      case (JsPath(KeyPathNode(key) :: Nil), value) =>
        objectMap.put(key, value)
        true
      case _ =>
        false
    }
    if (isSimpleObject) {
      JsObject(objectMap)
    } else {
      pathValues.foldLeft(JsObject.empty) { case (obj, (path, value)) =>
        obj.deepMerge(buildSubPath(path, value))
      }
    }
  }
}

/**
 * Path to a [[JsValue]];
 * As for path to file on FS, there may not be any matching value
 * in the parsed JSON.
 */
case class JsPath(path: List[PathNode] = List()) {
  def \(child: String) = JsPath(path :+ KeyPathNode(child))
  def \(child: Symbol) = JsPath(path :+ KeyPathNode(child.name))

  def \\(child: String) = JsPath(path :+ RecursiveSearch(child))
  def \\(child: Symbol) = JsPath(path :+ RecursiveSearch(child.name))

  def apply(idx: Int): JsPath = JsPath(path :+ IdxPathNode(idx))
  def \(idx: Int): JsPath     = apply(idx)

  def apply(json: JsValue): List[JsValue] = path.foldLeft(List(json))((s, p) => s.flatMap(p.apply))

  private lazy val PathMissingError = JsError(Seq(this -> JsonValidationError.PathMissing))

  def asSingleJsResult(json: JsValue): JsResult[JsValue] = path match {
    // Fast path, the most common place that this is invoked is by read, eg:
    // (__ \ "foo").read[Foo]
    // This fast path increases the performance of that operation as tested by JsonDeserialize_01_List by 35%
    case List(KeyPathNode(key)) =>
      json match {
        case JsObject(underlying) =>
          underlying.get(key) match {
            case Some(value) => JsSuccess(value)
            case None        => PathMissingError
          }
        case _ => PathMissingError
      }
    case _ =>
      this(json) match {
        case Nil      => PathMissingError
        case List(js) => JsSuccess(js)
        case _ :: _   => JsError(Seq(this -> Seq(JsonValidationError("error.path.result.multiple"))))
      }
  }

  def asSingleJson(json: JsValue): JsLookupResult = path match {
    // Fast path, the most common place that this is invoked is by readNullable, eg:
    // (__ \ "foo").readNullable[Foo]
    // This fast path increases the performance of that operation as tested by JsonDeserialize_02_Nullable by 82%
    case List(KeyPathNode(key)) =>
      json match {
        case JsObject(underlying) =>
          underlying.get(key) match {
            case Some(value) => JsDefined(value)
            case None        => JsLookupResult.PathMissing
          }
        case _ => JsLookupResult.PathMissing
      }
    case _ =>
      this(json) match {
        case Nil      => JsLookupResult.PathMissing
        case List(js) => JsDefined(js)
        case _ :: _   => JsUndefined("error.path.result.multiple")
      }
  }

  def applyTillLast(json: JsValue): Either[JsError, JsResult[JsValue]] = {
    @annotation.tailrec
    def step(path: List[PathNode], json: JsValue): Either[JsError, JsResult[JsValue]] = path match {
      case Nil => Right(JsSuccess(json))
      case List(node) =>
        node(json) match {
          case Nil      => Right(PathMissingError)
          case List(js) => Right(JsSuccess(js))
          case _ :: _   => Right(JsError(Seq(this -> Seq(JsonValidationError("error.path.result.multiple")))))
        }
      case head :: tail =>
        head(json) match {
          case Nil      => Left(PathMissingError)
          case List(js) => step(tail, js)
          case _ :: _   => Left(JsError(Seq(this -> Seq(JsonValidationError("error.path.result.multiple")))))
        }
    }

    step(path, json)
  }

  override def toString = path.mkString
  def toJsonString      = path.foldLeft("obj")((acc, p) => acc + p.toJsonString)

  def compose(other: JsPath) = JsPath(path ++ other.path)
  def ++(other: JsPath)      = this.compose(other)

  /**
   * Simple Prune for simple path and only JsObject
   */
  def prune(js: JsValue) = {
    def stepNode(json: JsObject, node: PathNode): JsResult[JsObject] = {
      node match {
        case KeyPathNode(key) => JsSuccess(json - key)
        case _                => JsError(JsPath(), JsonValidationError("error.expected.keypathnode"))
      }
    }

    def filterPathNode(json: JsObject, node: PathNode, value: JsValue): JsResult[JsObject] = {
      node match {
        case KeyPathNode(key) => JsSuccess(JsObject(json.fields.filterNot(_._1 == key)) ++ JsObject(Seq(key -> value)))
        case _                => JsError(JsPath(), JsonValidationError("error.expected.keypathnode"))
      }
    }

    def step(json: JsObject, lpath: JsPath): JsResult[JsObject] = {
      lpath.path match {
        case Nil     => JsSuccess(json)
        case List(p) => stepNode(json, p).repath(lpath)
        case head :: tail =>
          head(json) match {
            case Nil => JsError(lpath, JsonValidationError("error.path.missing"))
            case List(js) =>
              js match {
                case o: JsObject =>
                  step(o, JsPath(tail)).repath(lpath).flatMap(value => filterPathNode(json, head, value))
                case _ => JsError(lpath, JsonValidationError("error.expected.jsobject"))
              }
            case h :: t => JsError(lpath, JsonValidationError("error.path.result.multiple"))
          }
      }
    }

    js match {
      case o: JsObject =>
        step(o, this) match {
          case s @ JsSuccess(_: JsObject, _) => s.copy(path = this)
          case e                             => e
        }
      case _ =>
        JsError(this, JsonValidationError("error.expected.jsobject"))
    }
  }

  /** Reads a T at JsPath */
  def read[T](implicit r: Reads[T]): Reads[T] = Reads.at[T](this)(r)

  /** Reads a T at JsPath */
  def readWithDefault[T](defaultValue: => T)(implicit r: Reads[T]): Reads[T] =
    Reads.withDefault[T](this, defaultValue)

  /**
   * Reads a Option[T] search optional or nullable field at JsPath (field not found or null is None
   * and other cases are Error).
   *
   * It runs through JsValue following all JsPath nodes on JsValue:
   * - If any node in JsPath is not found => returns None
   * - If any node in JsPath is found with value "null" => returns None
   * - If the entire path is found => applies implicit Reads[T]
   */
  def readNullable[T](implicit r: Reads[T]): Reads[Option[T]] = Reads.nullable[T](this)(r)

  /**
   * Reads an Option[T] search optional or nullable field at JsPath (field not found replaced by
   * default value, null is None and other cases are Error).
   *
   * It runs through JsValue following all JsPath nodes on JsValue except last node:
   * - If any node in JsPath is not found => returns default value
   * - If any node in JsPath is found with value "null" => returns None
   * - If the entire path is found => applies implicit Reads[T]
   */
  def readNullableWithDefault[T](defaultValue: => Option[T])(implicit r: Reads[T]): Reads[Option[T]] =
    Reads.nullableWithDefault[T](this, defaultValue)(r)

  /**
   * Reads a T at JsPath using the explicit Reads[T] passed by name which is useful in case of
   * recursive case classes for ex.
   *
   * {{{
   * import play.api.libs.functional.syntax._
   * import play.api.libs.json.{ Reads, __ }
   *
   * case class User(id: Long, name: String, friend: User)
   *
   * implicit lazy val UserReads: Reads[User] = (
   *   (__ \ 'id).read[Long] and
   *   (__ \ 'name).read[String] and
   *   (__ \ 'friend).lazyRead(UserReads)
   * )(User.apply _)
   * }}}
   */
  def lazyRead[T](r: => Reads[T]): Reads[T] = Reads(js => Reads.at[T](this)(r).reads(js))

  /**
   * Reads lazily a Option[T] search optional or nullable field at JsPath using the explicit Reads[T]
   * passed by name which is useful in case of recursive case classes for ex.
   *
   * {{{
   * import play.api.libs.functional.syntax._
   * import play.api.libs.json.{ Reads, __ }
   *
   * case class User(id: Long, name: String, friend: Option[User])
   *
   * implicit lazy val UserReads: Reads[User] = (
   *   (__ \ 'id).read[Long] and
   *   (__ \ 'name).read[String] and
   *   (__ \ 'friend).lazyReadNullable(UserReads)
   * )(User.apply _)
   * }}}
   */
  def lazyReadNullable[T](r: => Reads[T]): Reads[Option[T]] = Reads(js => Reads.nullable[T](this)(r).reads(js))

  /** Pure Reads doesn't read anything but creates a JsObject based on JsPath with the given T value */
  def read[T](t: T) = Reads.pure(f = t)

  /** Writes a T at given JsPath */
  def write[T](implicit w: Writes[T]): OWrites[T] = Writes.at[T](this)(w)

  /**
   * Writes a Option[T] at given JsPath
   * If None => doesn't write the field (never writes null actually)
   * else => writes the field using implicit Writes[T]
   */
  def writeNullable[T](implicit w: Writes[T]): OWrites[Option[T]] = Writes.nullable[T](this)(w)

  /**
   * Writes a Option[T] at given JsPath
   * If None => writes 'null'
   * else => writes the field using implicit Writes[T]
   */
  def writeOptionWithNull[T](implicit w: Writes[T]): OWrites[Option[T]] =
    Writes.at[Option[T]](this)(Writes.optionWithNull[T](w))

  /**
   * Writes a T at JsPath using the explicit Writes[T] passed by name which is useful in case of
   * recursive case classes for ex
   *
   * {{{
   * import play.api.libs.functional.syntax._
   * import play.api.libs.json.{ Writes, __ }
   *
   * case class User(id: Long, name: String, friend: User)
   *
   * implicit lazy val UserWrites: Writes[User] = (
   *   (__ \ 'id).write[Long] and
   *   (__ \ 'name).write[String] and
   *   (__ \ 'friend).lazyWrite(UserWrites)
   * )(unlift(User.unapply))
   * }}}
   */
  def lazyWrite[T](w: => Writes[T]): OWrites[T] =
    OWrites((t: T) => Writes.at[T](this)(w).writes(t))

  /**
   * Writes a Option[T] at JsPath using the explicit Writes[T] passed by name which is useful in case of
   * recursive case classes for ex
   *
   * Please note that it's not writeOpt to be coherent with readNullable
   *
   * {{{
   * import play.api.libs.functional.syntax._
   * import play.api.libs.json.{ Writes, __ }
   *
   * case class User(id: Long, name: String, friend: Option[User])
   *
   * implicit lazy val UserWrites: Writes[User] = (
   *   (__ \ 'id).write[Long] and
   *   (__ \ 'name).write[String] and
   *   (__ \ 'friend).lazyWriteNullable(UserWrites)
   * )(unlift(User.unapply))
   * }}}
   */
  def lazyWriteNullable[T](w: => Writes[T]): OWrites[Option[T]] =
    OWrites((t: Option[T]) => Writes.nullable[T](this)(w).writes(t))

  /** Writes a pure value at given JsPath */
  def write[T](t: T)(implicit w: Writes[T]): OWrites[JsValue] = Writes.pure(this, t)

  /** Reads/Writes a T at JsPath using provided implicit Format[T] */
  def format[T](implicit f: Format[T]): OFormat[T] = Format.at[T](this)(f)

  /** Reads/Writes a T at JsPath using provided implicit Format[T] with fallback to default value */
  def formatWithDefault[T](defaultValue: => T)(implicit f: Format[T]): OFormat[T] = {
    Format.withDefault[T](this, defaultValue)(f)
  }

  /** Reads/Writes a T at JsPath using provided explicit Reads[T] and implicit Writes[T] */
  def format[T](r: Reads[T])(implicit w: Writes[T]): OFormat[T] = Format.at[T](this)(Format(r, w))

  /** Reads/Writes a T at JsPath using provided explicit Writes[T] and implicit Reads[T] */
  def format[T](w: Writes[T])(implicit r: Reads[T]): OFormat[T] = Format.at[T](this)(Format(r, w))

  /**
   * Reads/Writes a T at JsPath using provided implicit Reads[T] and Writes[T]
   *
   * Please note we couldn't call it "format" to prevent conflicts
   */
  def rw[T](implicit r: Reads[T], w: Writes[T]): OFormat[T] = Format.at[T](this)(Format(r, w))

  /**
   * Reads/Writes a Option[T] (optional or nullable field) at given JsPath
   *
   * @see JsPath.readNullable to see behavior in reads
   * @see JsPath.writeNullable to see behavior in writes
   */
  def formatNullable[T](implicit f: Format[T]): OFormat[Option[T]] = Format.nullable[T](this)(f)

  /**
   * Reads/Writes a Option[T] (nullable field) at given JsPath
   *
   * @see [[JsPath.readNullableWithDefault]] to see behavior in reads
   * @see [[JsPath.writeNullable]] to see behavior in writes
   */
  def formatNullableWithDefault[T](defaultValue: => Option[T])(implicit f: Format[T]): OFormat[Option[T]] = {
    Format.nullableWithDefault[T](this, defaultValue)(f)
  }

  /**
   * Lazy Reads/Writes a T at given JsPath using implicit Format[T]
   * (useful in case of recursive case classes).
   *
   * @see JsPath.lazyReadNullable to see behavior in reads
   * @see JsPath.lazyWriteNullable to see behavior in writes
   */
  def lazyFormat[T](f: => Format[T]): OFormat[T] = OFormat[T](lazyRead(f), lazyWrite(f))

  /**
   * Lazy Reads/Writes a Option[T] (optional or nullable field) at given JsPath using implicit Format[T]
   * (useful in case of recursive case classes).
   *
   * @see JsPath.lazyReadNullable to see behavior in reads
   * @see JsPath.lazyWriteNullable to see behavior in writes
   */
  def lazyFormatNullable[T](f: => Format[T]): OFormat[Option[T]] =
    OFormat[Option[T]](lazyReadNullable(f), lazyWriteNullable(f))

  /**
   * Lazy Reads/Writes a T at given JsPath using explicit Reads[T] and Writes[T]
   * (useful in case of recursive case classes).
   *
   * @see JsPath.lazyReadNullable to see behavior in reads
   * @see JsPath.lazyWriteNullable to see behavior in writes
   */
  def lazyFormat[T](r: => Reads[T], w: => Writes[T]): OFormat[T] = OFormat[T](lazyRead(r), lazyWrite(w))

  /**
   * Lazy Reads/Writes a Option[T] (optional or nullable field) at given JsPath using explicit Reads[T] and Writes[T]
   * (useful in case of recursive case classes).
   *
   * @see JsPath.lazyReadNullable to see behavior in reads
   * @see JsPath.lazyWriteNullable to see behavior in writes
   */
  def lazyFormatNullable[T](r: => Reads[T], w: => Writes[T]): OFormat[Option[T]] =
    OFormat[Option[T]](lazyReadNullable(r), lazyWriteNullable(w))

  private val self = this

  object json {

    /**
     * `(__ \ 'key).json.pick[A <: JsValue]` is a `Reads[A]` that:
     * - picks the given value at the given `JsPath` (WITHOUT THE PATH) from the input JS
     * - validates this element as an object of type A (inheriting JsValue)
     * - returns a `JsResult[A]`
     *
     * Useful to pick a typed JsValue at a given JsPath
     *
     * Example:
     *
     * {{{
     * import play.api.libs.json.{ Json, JsNumber, __ }
     *
     * val js = Json.obj("key1" -> "value1", "key2" -> 123)
     * js.validate((__ \ 'key2).json.pick[JsNumber])
     * // => JsSuccess(JsNumber(123),/key2)
     * }}}
     */
    def pick[A <: JsValue](implicit r: Reads[A]): Reads[A] = Reads.jsPick(self)

    /**
     * `(__ \ 'key).json.pick` is a `Reads[JsValue]` that:
     * - picks the given value at the given `JsPath` (WITHOUT THE PATH) from the input JS
     * - validates this element as an object of type [[JsValue]]
     * - returns a `JsResult[JsValue]`
     *
     * Useful to pick a [[JsValue]] at a given `JsPath`
     *
     * Example:
     *
     * {{{
     * import play.api.libs.json.{ Json, __ }
     *
     * val js = Json.obj("key1" -> "value1", "key2" -> "value2")
     * js.validate((__ \ 'key2).json.pick)
     * // => JsSuccess("value2",/key2)
     * }}}
     */
    def pick: Reads[JsValue] = pick[JsValue]

    /**
     * `(__ \ 'key).json.pickBranch[A <: JsValue](readsOfA)` is a `Reads[JsObject]` that:
     * - copies the given branch (`JsPath` + relative [[JsValue]]) from the input JS at this given `JsPath`
     * - validates this relative `JsValue` as an object of type A (inheriting `JsValue`) potentially modifying it
     * - creates a [[JsObject]] from `JsPath` and validated `JsValue`
     * - returns a `JsResult[JsObject]`
     *
     * Useful to create/validate an [[JsObject]] from a single `JsPath` (potentially modifying it)
     *
     * Example:
     *
     * {{{
     * import play.api.libs.json.{ Json, JsString, __ }
     *
     * val js = Json.obj("key1" -> "value1", "key2" -> Json.obj( "key21" -> "value2") )
     * js.validate( (__ \ 'key2).json.pickBranch[JsString]( (__ \ 'key21).json.pick[JsString].map( (js: JsString) => JsString(js.value ++ "3456") ) ) )
     * // => JsSuccess({"key2":"value23456"},/key2/key21)
     * }}}
     */
    def pickBranch[A <: JsValue](reads: Reads[A]): Reads[JsObject] = Reads.jsPickBranch[A](self)(reads)

    /**
     * `(__ \ 'key).json.pickBranch` is a `Reads[JsObject]` that:
     * - copies the given branch (`JsPath` + relative [[JsValue]]) from the input JS at this given `JsPath`
     * - creates a `JsObject` from `JsPath` and `JsValue`
     * - returns a `JsResult[JsObject]`
     *
     * Useful to create/validate an [[JsObject]] from a single `JsPath` (potentially modifying it)
     *
     * Example:
     *
     * {{{
     * import play.api.libs.json.{ Json, __ }
     *
     * val js = Json.obj("key1" -> "value1", "key2" -> Json.obj( "key21" -> "value2") )
     * js.validate( (__ \ 'key2).json.pickBranch )
     * // => JsSuccess({"key2":{"key21":"value2"}},/key2)
     * }}}
     */
    def pickBranch: Reads[JsObject] = Reads.jsPickBranch[JsValue](self)

    /**
     * `(__ \ 'key).put(fixedValue)` is a `Reads[JsObject]` that:
     * - creates a [[JsObject]] setting A (inheriting [[JsValue]]) at given `JsPath`
     * - returns a `JsResult[JsObject]`
     *
     * This `Reads` doesn't care about the input JS and is mainly used to set a fixed at a given `JsPath`
     * Please that `A` is passed by name allowing to use an expression reevaluated at each time.
     *
     * Example:
     *
     * {{{
     * import play.api.libs.json.{ Json, JsNumber, __ }
     *
     * val js = Json.obj("key1" -> "value1", "key2" -> "value2")
     * js.validate( (__ \ 'key3).json.put( { JsNumber((new java.util.Date).getTime()) } ) )
     * // => JsSuccess({"key3":1376419773171},)
     * }}}
     */
    def put(a: => JsValue): Reads[JsObject] = Reads.jsPut(self, a)

    /**
     * `(__ \ 'key).json.copyFrom(reads)` is a `Reads[JsObject]` that:
     * - copies a [[JsValue]] using passed `Reads[A]`
     * - creates a new branch from `JsPath` and copies previous value into it
     *
     * Useful to copy a value from a JSON branch into another branch.
     *
     * Example:
     *
     * {{{
     * import play.api.libs.json.{ Json, __ }
     *
     * val js = Json.obj("key1" -> "value1", "key2" -> "value2")
     * js.validate( (__ \ 'key3).json.copyFrom((__ \ 'key2).json.pick))
     * // => JsSuccess({"key3":"value2"},/key2)
     * }}}
     */
    def copyFrom[A <: JsValue](reads: Reads[A]): Reads[JsObject] = Reads.jsCopyTo(self)(reads)

    /**
     * `(__ \ 'key).json.update(reads)` is the most complex `Reads[JsObject]` but the most powerful:
     * - copies the whole `JsValue => A`
     * - applies the passed `Reads[A]` on `JsValue => B`
     * - deep merges both `JsValues (A ++ B)` so `B` overwrites `A` identical branches
     *
     * Please note that if you have prune a branch in `B`, it is still in `A` so you'll see it in the result
     *
     * Example:
     *
     * {{{
     * import play.api.libs.json.{ Json, JsString, __ }
     *
     * val js = Json.obj("key1" -> "value1", "key2" -> "value2")
     * js.validate(__.json.update((__ \ 'key3).json.put(JsString("value3"))))
     * // => JsSuccess({"key1":"value1","key2":"value2","key3":"value3"},)
     * }}}
     */
    def update[A <: JsValue](reads: Reads[A]): Reads[JsObject] = Reads.jsUpdate(self)(reads)

    /**
     * `(__ \ 'key).json.prune` is `Reads[JsObject]` that prunes the branch and returns remaining [[JsValue]].
     *
     * Example:
     *
     * {{{
     * import play.api.libs.json.{ Json, __ }
     *
     * val js = Json.obj("key1" -> "value1", "key2" -> "value2")
     * js.validate( (__ \ 'key2).json.prune )
     * // => JsSuccess({"key1":"value1"},/key2)
     * }}}
     */
    def prune: Reads[JsObject] = Reads.jsPrune(self)
  }
}
