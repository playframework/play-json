/*
 * Copyright (C) 2009-2020 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import java.io.InputStream

/**
 * @define jsonParam @param json the JsValue to convert
 * @define returnStringRepr A String with the json representation
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
   * Parses some bytes representing a JSON input,
   * and returns it as a [[JsValue]].
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
   * import play.api.libs.json.Json
   *
   * val input = Json.obj(
   *   "field1" -> Json.obj(
   *     "field11" -> "value11",
   *     "field12" -> Json.arr("alpha", 123L)
   *   )
   * )
   *
   * Json.stringify(input)
   * // => {"field1":{"field11":"value11","field12":["alpha",123]}}
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
   * import play.api.libs.json.{ Json, JsString }
   *
   * Json.asciiStringify(JsString("some\\u005Ctext\\u005C"))
   * // => "some\\u005Ctext\\u005C"
   *
   * Json.stringify(JsString("some\\u005Ctext\\u005C"))
   * // => "sometext"
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
   * import play.api.libs.json.Json
   *
   * val res0 = Json.obj(
   *   "field1" -> Json.obj(
   *     "field11" -> "value11",
   *     "field12" -> Json.arr("alpha", 123L)
   *   )
   * )
   * // => {"field1":{"field11":"value11","field12":["alpha",123]}}
   *
   * Json.prettyPrint(res0)
   * // =>
   * // {
   * //   "field1" : {
   * //     "field11" : "value11",
   * //     "field12" : [ "alpha", 123 ]
   * //   }
   * // }
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
   * @tparam T the type of the value to be written as JSON
   * @param o the value to convert as JSON
   */
  def toJson[T](o: T)(implicit tjs: Writes[T]): JsValue

  /**
   * Converts any object writeable value to a [[JsObject]].
   *
   * A value is writeable as an object,
   * if a [[OWrites]] implicit is available for its type.
   *
   * @tparam T the type of the value to be written as `JsObject`
   * @param o the value to convert as JSON object
   */
  def toJsObject[T](o: T)(implicit tjs: OWrites[T]): JsObject

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
 * @define macroOptions @tparam Opts the compile-time options
 * @define macroTypeParam @tparam A the type for which the handler must be materialized
 * @define macroWarning If any missing implicit is discovered, compiler will break with corresponding error.
 */
object Json extends JsonFacade with JsMacros with JsValueMacros {
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

  def toJsObject[T](o: T)(implicit tjs: OWrites[T]): JsObject = tjs.writes(o)

  def fromJson[T](json: JsValue)(implicit fjs: Reads[T]): JsResult[T] = fjs.reads(json)

  /**
   * Next is the trait that allows Simplified Json syntax :
   *
   * Example:
   *
   * {{{
   * import play.api.libs.json._
   *
   * JsObject(Seq(
   *    "key1" -> JsString("value"),
   *    "key2" -> JsNumber(123),
   *    "key3" -> JsObject(Seq("key31" -> JsString("value31")))
   * )) == Json.obj(
   *   "key1" -> "value", "key2" -> 123, "key3" -> Json.obj("key31" -> "value31"))
   *
   * JsArray(Seq(JsString("value"), JsNumber(123), JsBoolean(true))) == Json.arr("value", 123, true)
   * }}}
   *
   * There is an implicit conversion from any Type with a Json Writes to JsValueWrapper
   * which is an empty trait that shouldn't end into unexpected implicit conversions.
   */
  sealed trait JsValueWrapper

  private case class JsValueWrapperImpl(field: JsValue) extends JsValueWrapper

  import scala.language.implicitConversions

  implicit def toJsFieldJsValueWrapper[T](field: T)(implicit w: Writes[T]): JsValueWrapper =
    JsValueWrapperImpl(w.writes(field))

  def obj(fields: (String, JsValueWrapper)*): JsObject = JsObject(fields.map(f => (f._1, unwrap(f._2))))
  def arr(items: JsValueWrapper*): JsArray             = JsArray(items.iterator.map(unwrap).toArray[JsValue])

  // Passed nulls will typecheck without needing the implicit conversion, so they need to checked at runtime
  private def unwrap(wrapper: JsValueWrapper) = wrapper match {
    case null                      => JsNull
    case JsValueWrapperImpl(value) => value
  }

  /**
   * Creates a `Format[E]` by automatically creating Reads[E] and Writes[E] for any Enumeration E
   *
   * {{{
   * import play.api.libs.json.{ Format, Json }
   *
   * object DayOfWeek extends Enumeration {
   *
   *  type DayOfWeek = Value
   *
   *  val Mon = Value("Monday")
   *  val Tue = Value("Tuesday")
   *  val Wed = Value("Wednesday")
   *  // etc.
   *
   *   implicit val format1: Format[DayOfWeek] = Json.formatEnum(DayOfWeek)
   *   // or 'this' if defining directly in Enum
   *   implicit val format2: Format[DayOfWeek] = Json.formatEnum(this)
   * }
   * }}}
   *
   * `Json.toJson(Mon)` will produce `"Monday"`.
   *
   * @param enum Enumeration object
   * @tparam E type of Enum
   */
  def formatEnum[E <: Enumeration](`enum`: E): Format[`enum`.Value] =
    Format(Reads.enumNameReads(`enum`), Writes.enumNameWrites[`enum`.type])

  /**
   * JSON facade with some macro options.
   *
   * $macroOptions
   *
   * @define macroWarning If any missing implicit is discovered, compiler will break with corresponding error.
   * @define macroTypeParam @tparam A the type for which the handler must be materialized
   */
  final class WithOptions[Opts <: MacroOptions](val config: JsonConfiguration.Aux[Opts])
      extends JsonFacade
      with JsMacrosWithOptions {
    def this() = this(JsonConfiguration.default)

    @inline def parse(input: String): JsValue       = Json.parse(input)
    @inline def parse(input: InputStream): JsValue  = Json.parse(input)
    @inline def parse(input: Array[Byte]): JsValue  = Json.parse(input)
    @inline def stringify(json: JsValue): String    = Json.stringify(json)
    @inline def toBytes(json: JsValue): Array[Byte] = Json.toBytes(json)

    @inline def asciiStringify(json: JsValue): String =
      Json.asciiStringify(json)

    @inline def prettyPrint(json: JsValue): String = Json.prettyPrint(json)
    @inline def toJson[T](o: T)(implicit tjs: Writes[T]): JsValue =
      Json.toJson[T](o)

    @inline def toJsObject[T](o: T)(implicit tjs: OWrites[T]): JsObject =
      Json.toJsObject[T](o)

    @inline def fromJson[T](json: JsValue)(implicit fjs: Reads[T]): JsResult[T] = Json.fromJson[T](json)

    @inline def obj(fields: (String, JsValueWrapper)*): JsObject = Json.obj(fields: _*)

    @inline def arr(items: JsValueWrapper*): JsArray = Json.arr(items: _*)
  }

  /**
   * Returns a [[JsonFacade]] using the current JSON configuration.
   *
   * @tparam C the type of compile-time configuration
   *
   * {{{
   * import play.api.libs.json.{ Json, Reads }
   *
   * case class Foo(v: String)
   *
   * // Materializes a `Reads[Foo]`,
   * // with the configuration resolved at compile time
   * val r: Reads[Foo] = Json.configured.reads[Foo]
   * }}}
   */
  def configured[Opts <: MacroOptions](implicit config: JsonConfiguration.Aux[Opts]) = new WithOptions[Opts](config)

  /**
   * Returns an inference context to call the JSON macros,
   * using explicit compile-time options.
   *
   * $macroOptions
   */
  def using[Opts <: MacroOptions] = new WithOptions[Opts](JsonConfiguration[Opts]())

  /**
   * Compile-time base options for macro usage.
   *
   * {{{
   * import play.api.libs.json.Json
   *
   * case class Foo(v: String)
   *
   * Json.using[Json.MacroOptions].format[Foo]
   * // equivalent to Json.format[Foo]
   * }}}
   */
  sealed trait MacroOptions

  object MacroOptions {

    /**
     * Defines the default macro options if no type is supplied.
     *
     * Since you can't have defaults for a type parameter (unless it's a contravariant type, it will default to
     * Nothing), we supply the default via an implicit parameter.
     */
    trait Default[O <: Json.MacroOptions]

    trait LowPriorityDefaultImplicits {

      /**
       * Low priority implicit used when some explicit Json.MacroOptions instance is passed.
       */
      implicit def lowPriorityDefault[O <: Json.MacroOptions]: Default[O] = new Default[O] {}
    }

    object Default extends LowPriorityDefaultImplicits {

      /**
       * This will be the default that's passed when no MacroOptions is passed.
       */
      implicit object macroOptionsDefault extends Default[Json.MacroOptions]
    }
  }

  /**
   * Flag to indicate the macros can use the type default values
   * (e.g. default values for the case class parameters)
   * when applicable.
   *
   * {{{
   * import play.api.libs.json._, Json._
   *
   * type Opts = MacroOptions with DefaultValues
   * }}}
   */
  trait DefaultValues { self: MacroOptions =>
  }

  /**
   * Alias for `MacroOptions with DefaultValues`
   *
   * {{{
   * import play.api.libs.json.Json
   *
   * Json.using[Json.WithDefaultValues]
   * }}}
   */
  type WithDefaultValues = MacroOptions with DefaultValues
}
