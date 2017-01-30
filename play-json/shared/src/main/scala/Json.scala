/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package play.api.libs.json

import java.io.InputStream

/**
 * Helper functions to handle JsValues.
 */
object Json {

  /**
   * Parse a String representing a json, and return it as a JsValue.
   *
   * @param input a String to parse
   * @return the JsValue representing the string
   */
  def parse(input: String): JsValue = StaticBinding.parseJsValue(input)

  /**
   * Parse an InputStream representing a json, and return it as a JsValue.
   *
   * @param input as InputStream to parse
   * @return the JsValue representing the InputStream
   */
  def parse(input: InputStream): JsValue = StaticBinding.parseJsValue(input)

  /**
   * Parse a byte array representing a json, and return it as a JsValue.
   *
   * The character encoding used will be automatically detected as UTF-8, UTF-16 or UTF-32, as per the heuristics in
   * RFC-4627.
   *
   * @param input a byte array to parse
   * @return the JsValue representing the byte array
   */
  def parse(input: Array[Byte]): JsValue = StaticBinding.parseJsValue(input)

  /**
   * Convert a JsValue to its string representation.
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
   * @param json the JsValue to convert
   * @return a String with the json representation
   */
  def stringify(json: JsValue): String =
    StaticBinding.generateFromJsValue(json, false)

  /**
   * Converts a JsValue directly to an array of bytes (using UTF-8 encoding)
   *
   * @param json the JsValue to convert
   * @return an Array[Byte] with the UTF-8-encoded JSON representation
   */
  def toBytes(json: JsValue): Array[Byte] = StaticBinding.toBytes(json)

  //We use unicode \u005C for a backlash in comments, because Scala will replace unicode escapes during lexing
  //anywhere in the program.
  /**
   * Converts a JsValue to its string representation, escaping all non-ascii characters using \u005CuXXXX syntax.
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
   * @param json the JsValue to convert
   * @return a String with the json representation with all non-ascii characters escaped.
   */
  def asciiStringify(json: JsValue): String =
    StaticBinding.generateFromJsValue(json, true)

  /**
   * Convert a JsValue to its pretty string representation using default
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
   * @param json the JsValue to convert
   * @return a String with the json representation
   */
  def prettyPrint(json: JsValue): String = StaticBinding.prettyPrint(json)

  /**
   * Provided a Writes implicit for its type is available, convert any object into a JsValue.
   *
   * @param o Value to convert in Json.
   */
  def toJson[T](o: T)(implicit tjs: Writes[T]): JsValue = tjs.writes(o)

  /**
   * Provided a Reads implicit for that type is available, convert a JsValue to any type.
   *
   * @param json Json value to transform as an instance of T.
   */
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

  def arr(fields: JsValueWrapper*): JsArray = JsArray(fields.map(_.asInstanceOf[JsValueWrapperImpl].field))

  /**
   * Experimental JSON extensions to replace asProductXXX by generating
   * Reads[T]/Writes[T]/Format[T] from case class at COMPILE time using
   * new Scala 2.10 macro & reflection features.
   */
  import language.experimental.macros

  /**
   * Creates a Reads[T] by resolving case class fields & required implicits at COMPILE-time.
   *
   * If any missing implicit is discovered, compiler will break with corresponding error.
   * {{{
   *   import play.api.libs.json.Json
   *
   *   case class User(userName: String, age: Int)
   *
   *   implicit val userReads = Json.reads[User]
   *   // macro-compiler replaces Json.reads[User] by injecting into compile chain
   *   // the exact code you would write yourself. This is strictly equivalent to:
   *   implicit val userReads = (
   *      (__ \ implicitly[JsonConfiguration].naming("userName")).read[String] and
   *      (__ \ implicitly[JsonConfiguration].naming("age")).read[Int]
   *   )(User)
   * }}}
   */
  def reads[A]: Reads[A] = macro JsMacroImpl.readsImpl[A]

  /**
   * Creates a Writes[T] by resolving case class fields & required implicits at COMPILE-time
   *
   * If any missing implicit is discovered, compiler will break with corresponding error.
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
  def writes[A]: OWrites[A] = macro JsMacroImpl.writesImpl[A]

  /**
   * Creates a Format[T] by resolving case class fields & required implicits at COMPILE-time
   *
   * If any missing implicit is discovered, compiler will break with corresponding error.
   * {{{
   *   import play.api.libs.json.Json
   *
   *   case class User(userName: String, age: Int)
   *
   *   implicit val userWrites = Json.format[User]
   *   // macro-compiler replaces Json.format[User] by injecting into compile chain
   *   // the exact code you would write yourself. This is strictly equivalent to:
   *   implicit val userWrites = (
   *      (__ \ implicitly[JsonConfiguration].naming("userName")).format[String] and
   *      (__ \ implicitly[JsonConfiguration].naming("age")).format[Int]
   *   )(User.apply, unlift(User.unapply))
   * }}}
   */
  def format[A]: OFormat[A] = macro JsMacroImpl.formatImpl[A]

}
