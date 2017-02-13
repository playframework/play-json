/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package play.api.libs.json

import java.io.InputStream

/**
 * @define jsonParam @param json the JsValue to convert
 * @define returnStringRepr A String with the json representation
 * @define readDescription Creates a `Reads[T]` by resolving, at compile-time, the case class fields or sealed family, and the required implicits.
 * @define handlerTypeParam @tparam A the type for which the handler must be materialized
 */
sealed trait JsonFacade {
  /**
   * Parses a String representing a JSON input, and returns it as a [[JsValue]].
   *
   * @param input the String to parse
   */
  def parse(input: String): JsValue

  /**
   * Parses a stream representing a JSON input, and returns it as a [[JsValue]].
   *
   * @param input the InputStream to parse
   */
  def parse(input: InputStream): JsValue

  /**
   * Parses some bytes representing a JSON input, and returns it as a [[JsValue]].
   *
   * The character encoding used will be automatically detected as UTF-8,
   * UTF-16 or UTF-32, as per the heuristics in RFC-4627.
   *
   * @param input the byte array to parse
   */
  def parse(input: Array[Byte]): JsValue

  /**
   * Converts a [[JsValue]] to its string representation.
   *
   * {{{
   * scala> Json.stringify(Json.obj(
   *   "field1" -> Json.obj(
   *     "field11" -> "value11",
   *     "field12" -> Json.arr("alpha", 123L)
   *   )
   * ))
   * res0: String = {"field1":{"field11":"value11","field12":["alpha",123]}}
   *
   * scala> Json.stringify(res0)
   * res1: String = {"field1":{"field11":"value11","field12":["alpha",123]}}
   * }}}
   *
   * $jsonParam
   * @return a String with the json representation
   */
  def stringify(json: JsValue): String

  /**
   * Converts a [[JsValue]] to bytes (using UTF-8 encoding).
   *
   * $jsonParam
   * @return an `Array[Byte]` representing the UTF-8-encoded JSON
   */
  def toBytes(json: JsValue): Array[Byte]

  /**
   * Converts a [[JsValue]] to its string representation,
   * escaping all non-ascii characters using `\u005CuXXXX` syntax.
   *
   * This is particularly useful when the output JSON will be executed as javascript, since JSON is not a strict
   * subset of javascript
   * (see <a href="http://timelessrepo.com/json-isnt-a-javascript-subset">JSON: The JavaScript subset that isn't</a>).
   *
   * {{{
   * scala> Json.asciiStringify(JsString("some\u005Cu2028text\u005Cu2029"))
   * res0: String = "some\u005Cu2028text\u005Cu2029"
   *
   * scala> Json.stringify(JsString("some\u005Cu2028text\u005Cu2029"))
   * res1: String = "sometext"
   * }}}
   *
   * $jsonParam
   * $returnStringRepr with all non-ascii characters escaped.
   */
  def asciiStringify(json: JsValue): String

  /**
   * Converts a [[JsValue]] to its pretty string representation using default
   * pretty printer (line feeds after each fields and 2-spaces indentation).
   *
   * {{{
   * scala> Json.stringify(Json.obj(
   *   "field1" -> Json.obj(
   *     "field11" -> "value11",
   *     "field12" -> Json.arr("alpha", 123L)
   *   )
   * ))
   * res0: String = {"field1":{"field11":"value11","field12":["alpha",123]}}
   *
   * scala> Json.prettyPrint(res0)
   * res1: String =
   * {
   *   "field1" : {
   *     "field11" : "value11",
   *     "field12" : [ "alpha", 123 ]
   *   }
   * }
   * }}}
   *
   * $jsonParam
   * $returnStringRepr.
   */
  def prettyPrint(json: JsValue): String

  /**
   * Converts any writeable value to a [[JsValue]].
   *
   * A value is writeable if a [[Writes]] implicit is available for its type.
   *
   * @param o the value to convert as JSON
   */
  def toJson[T](o: T)(implicit tjs: Writes[T]): JsValue

  /**
   * Converts a [[JsValue]] to a value of requested type `T`.
   *
   * @tparam T The type of conversion result,
   * only supported if a [[Reads]] implicit is available for.
   *
   * $jsonParam
   */
  def fromJson[T](json: JsValue)(implicit fjs: Reads[T]): JsResult[T]

  /**
   * Returns a [[JsObject]] with given fields.
   *
   * @param fields the object fields specified as pairs of name and value
   */
  def obj(fields: (String, Json.JsValueWrapper)*): JsObject

  /** Returns a [[JsArray]] with given items. */
  def arr(items: Json.JsValueWrapper*): JsArray

}

/**
 * Helper functions to handle JsValues.
 *
 * @define macroOptions @tparam O the compile-time options
 * @define macroWarning If any missing implicit is discovered, compiler will break with corresponding error.
 * @define formatDescription Creates a `OFormat[T]` by resolving, at compile-time, the case class fields or sealed family, and the required implicits.
 */
object Json extends JsonFacade {

  def parse(input: String): JsValue = StaticBinding.parseJsValue(input)

  def parse(input: InputStream): JsValue = StaticBinding.parseJsValue(input)

  def parse(input: Array[Byte]): JsValue = StaticBinding.parseJsValue(input)

  def stringify(json: JsValue): String =
    StaticBinding.generateFromJsValue(json, false)

  def toBytes(json: JsValue): Array[Byte] = StaticBinding.toBytes(json)

  //We use unicode \u005C for a backlash in comments, because Scala will replace unicode escapes during lexing
  //anywhere in the program.
  def asciiStringify(json: JsValue): String =
    StaticBinding.generateFromJsValue(json, true)

  def prettyPrint(json: JsValue): String = StaticBinding.prettyPrint(json)

  def toJson[T](o: T)(implicit tjs: Writes[T]): JsValue = tjs.writes(o)

  def fromJson[T](json: JsValue)(implicit fjs: Reads[T]): JsResult[T] = fjs.reads(json)

  /**
   * Next is the trait that allows Simplified Json syntax :
   *
   * Example :
   * {{{
   * JsObject(Seq(
   *    "key1" -> JsString("value"),
   *    "key2" -> JsNumber(123),
   *    "key3" -> JsObject(Seq("key31" -> JsString("value31")))
   * )) == Json.obj( "key1" -> "value", "key2" -> 123, "key3" -> obj("key31" -> "value31"))
   *
   * JsArray(JsString("value"), JsNumber(123), JsBoolean(true)) == Json.arr( "value", 123, true )
   * }}}
   *
   * There is an implicit conversion from any Type with a Json Writes to JsValueWrapper
   * which is an empty trait that shouldn't end into unexpected implicit conversions.
   */
  sealed trait JsValueWrapper

  private case class JsValueWrapperImpl(field: JsValue) extends JsValueWrapper

  import scala.language.implicitConversions

  implicit def toJsFieldJsValueWrapper[T](field: T)(implicit w: Writes[T]): JsValueWrapper = JsValueWrapperImpl(w.writes(field))

  def obj(fields: (String, JsValueWrapper)*): JsObject = JsObject(fields.map(f => (f._1, f._2.asInstanceOf[JsValueWrapperImpl].field)))

  def arr(items: JsValueWrapper*): JsArray =
    JsArray(items.map(_.asInstanceOf[JsValueWrapperImpl].field))

  import language.experimental.macros

  /**
   * $readsDescription
   *
   * $macroWarning
   *
   * $handlerTypeParam
   *
   * {{{
   * import play.api.libs.json.Json
   *
   * case class User(userName: String, age: Int)
   *
   * implicit val userReads = Json.reads[User]
   * // macro-compiler replaces Json.reads[User] by injecting into compile chain
   * // the exact code you would write yourself. This is strictly equivalent to:
   * implicit val userReads = (
   *    (__ \ implicitly[JsonConfiguration].naming("userName")).read[String] and
   *    (__ \ implicitly[JsonConfiguration].naming("age")).read[Int]
   * )(User)
   * }}}
   */
  def reads[A]: Reads[A] = macro JsMacroImpl.readsImpl[A, MacroOptions]

  /** JSON facade with some macro options. */
  final class WithOptions[Opts <: MacroOptions]() extends JsonFacade {
    @inline def parse(input: String): JsValue = Json.parse(input)
    @inline def parse(input: InputStream): JsValue = Json.parse(input)
    @inline def parse(input: Array[Byte]): JsValue = Json.parse(input)
    @inline def stringify(json: JsValue): String = Json.stringify(json)
    @inline def toBytes(json: JsValue): Array[Byte] = Json.toBytes(json)

    @inline def asciiStringify(json: JsValue): String =
      Json.asciiStringify(json)

    @inline def prettyPrint(json: JsValue): String = Json.prettyPrint(json)
    @inline def toJson[T](o: T)(implicit tjs: Writes[T]): JsValue =
      Json.toJson[T](o)

    @inline def fromJson[T](json: JsValue)(implicit fjs: Reads[T]): JsResult[T] = Json.fromJson[T](json)

    @inline def obj(fields: (String, JsValueWrapper)*): JsObject = Json.obj(fields: _*)

    @inline def arr(items: JsValueWrapper*): JsArray = Json.arr(items: _*)

    /**
     * $readsDescription
     *
     * $macroWarning
     *
     * $handlerTypeParam
     *
     * {{{
     * import play.api.libs.json.Json
     *
     * case class User(userName: String, age: Int)
     *
     * implicit val userReads: Reads[User] =
     *   Json.using[Json.MacroOptions with Json.DefaultValues].reads[User]
     */
    def reads[A]: Reads[A] = macro JsMacroImpl.readsImpl[A, Opts]
  }

  /**
   * Returns an inference context to call the JSON macros,
   * using the current JSON configuration.
   */
  def configured[C <: JsonConfiguration.Aux[_ <: MacroOptions]](implicit config: C) = new WithOptions[config.Opts]()

  /**
   * Returns an inference context to call the JSON macros,
   * using explicit compile-time options.
   */
  def using[Opts <: MacroOptions] = new WithOptions[Opts]()

  /**
   * Creates a `OWrites[T]` by resolving, at compile-time, the case class fields or sealed family, and the required implicits.
   *
   * $macroWarning
   *
   * $handlerTypeParam
   *
   * {{{
   *   import play.api.libs.json.Json
   *
   *   case class User(userName: String, age: Int)
   *
   *   implicit val userWrites = Json.writes[User]
   *   // macro-compiler replaces Json.writes[User] by injecting into compile chain
   *   // the exact code you would write yourself. This is strictly equivalent to:
   *   implicit val userWrites = (
   *      (__ \ implicitly[JsonConfiguration].naming("userName")).write[String] and
   *      (__ \ implicitly[JsonConfiguration].naming("age")).write[Int]
   *   )(unlift(User.unapply))
   * }}}
   */
  def writes[A]: OWrites[A] = macro JsMacroImpl.writesImpl[A, MacroOptions]

  /**
   * $formatDescription
   *
   * $macroWarning
   *
   * $handlerTypeParam
   *
   * {{{
   * import play.api.libs.json.Json
   *
   * case class User(userName: String, age: Int)
   *
   * implicit val userWrites = Json.format[User]
   * // macro-compiler replaces Json.format[User] by injecting into compile chain
   * // the exact code you would write yourself. This is strictly equivalent to:
   * implicit val userWrites = (
   *    (__ \ implicitly[JsonConfiguration].naming("userName")).format[String] and
   *    (__ \ implicitly[JsonConfiguration].naming("age")).format[Int]
   * )(User.apply, unlift(User.unapply))
   * }}}
   */
  def format[A]: OFormat[A] = macro JsMacroImpl.formatImpl[A, MacroOptions]

  /**
   * Compile-time base options for macro usage.
   *
   * {{{
   * Json.formatOpts[Foo, Json.MacroOptions]
   * // equivalent to Json.format[Foo]
   * }}}
   */
  sealed trait MacroOptions

}
