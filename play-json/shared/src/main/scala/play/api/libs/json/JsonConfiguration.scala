/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

/** JSON configuration */
sealed trait JsonConfiguration {

  /** Compile-time options for the JSON macros */
  type Opts <: Json.MacroOptions

  /** Naming strategy for fields */
  def naming: JsonNaming

  /** Naming strategy for type names */
  def typeNaming: JsonNaming

  /** How options are handled by the macro */
  def optionHandlers: OptionHandlers

  /**
   * Name of the type discriminator field
   * (for sealed family; see [[JsonConfiguration$.defaultDiscriminator]])
   */
  def discriminator: String
}

object JsonConfiguration {
  type Aux[O <: Json.MacroOptions] = JsonConfiguration { type Opts = O }

  private final class Impl[O <: Json.MacroOptions](
      val naming: JsonNaming = JsonNaming.Identity,
      val optionHandlers: OptionHandlers = OptionHandlers.Default,
      val discriminator: String = defaultDiscriminator,
      val typeNaming: JsonNaming = JsonNaming.Identity
  ) extends JsonConfiguration {
    type Opts = O

    def this(naming: JsonNaming) = this(naming, OptionHandlers.Default, defaultDiscriminator)
    def this(naming: JsonNaming, optionHandlers: OptionHandlers) = this(naming, optionHandlers, defaultDiscriminator)
  }

  // These methods exist for binary compatibility, since Scala protected methods are public from a binary perspective.
  protected def apply(naming: JsonNaming): JsonConfiguration.Aux[Json.MacroOptions] = new Impl(naming)
  protected def apply(naming: JsonNaming, optionHandlers: OptionHandlers): JsonConfiguration.Aux[Json.MacroOptions] =
    new Impl(naming, optionHandlers)
  protected def default: JsonConfiguration.Aux[Json.MacroOptions] = apply()

  val defaultDiscriminator = "_type"

  /**
   * @param naming the naming strategy
   * @param optionHandlers handlers for option
   * @param discriminator See [[JsonConfiguration.discriminator]]
   * @param typeNaming See [[JsonConfiguration.typeNaming]]
   */
  def apply[Opts <: Json.MacroOptions: Json.MacroOptions.Default](
      naming: JsonNaming = JsonNaming.Identity,
      optionHandlers: OptionHandlers = OptionHandlers.Default,
      discriminator: String = defaultDiscriminator,
      typeNaming: JsonNaming = JsonNaming.Identity
  ): JsonConfiguration.Aux[Opts] = new Impl(naming, optionHandlers, discriminator, typeNaming)

  /** Default configuration instance */
  implicit def default[Opts <: Json.MacroOptions: Json.MacroOptions.Default]: JsonConfiguration.Aux[Opts] = apply()
}

/**
 * Naming strategy, to map each class property to the corresponding column.
 */
trait JsonNaming extends (String => String) {

  /**
   * Returns the column name for the class property.
   *
   * @param property the name of the case class property
   */
  def apply(property: String): String
}

/** Naming companion */
object JsonNaming {

  /**
   * For each class property, use the name
   * as is for its column (e.g. fooBar -> fooBar).
   */
  object Identity extends JsonNaming {
    def apply(property: String): String = property
    override val toString               = "Identity"
  }

  /**
   * For each class property, use the snake case equivalent
   * to name its column (e.g. fooBar -> foo_bar).
   */
  object SnakeCase extends JsonNaming {
    def apply(property: String): String = {
      val length            = property.length
      val result            = new StringBuilder(length * 2)
      var resultLength      = 0
      var wasPrevTranslated = false

      for (i <- 0.until(length)) {
        var c = property.charAt(i)
        if (i > 0 || c != '_') {
          if (Character.isUpperCase(c)) {
            // append a underscore if the previous result wasn't translated
            if (!wasPrevTranslated && resultLength > 0 && result.charAt(resultLength - 1) != '_') {
              result.append('_')
              resultLength += 1
            }
            c = Character.toLowerCase(c)
            wasPrevTranslated = true
          } else {
            wasPrevTranslated = false
          }
          result.append(c)
          resultLength += 1
        }
      }

      // builds the final string
      result.toString()
    }

    override val toString = "SnakeCase"
  }

  /**
   * For each class property, use the pascal case equivalent
   * to name its column (e.g. fooBar -> FooBar).
   */
  object PascalCase extends JsonNaming {
    def apply(property: String): String = property.capitalize

    override val toString = "PascalCase"
  }

  /** Naming using a custom transformation function. */
  def apply(transformation: String => String): JsonNaming = new JsonNaming {
    def apply(property: String): String = transformation(property)
  }
}

/** Configure how options should be handled */
trait OptionHandlers {
  def writeHandler[T](jsPath: JsPath)(implicit writes: Writes[T]): OWrites[Option[T]]
  def readHandler[T](jsPath: JsPath)(implicit r: Reads[T]): Reads[Option[T]]

  def readHandlerWithDefault[T](jsPath: JsPath, defaultValue: => Option[T])(implicit r: Reads[T]): Reads[Option[T]] = {
    jsPath.readNullableWithDefault(defaultValue)
  }

  final def formatHandler[T](jsPath: JsPath)(implicit format: Format[T]): OFormat[Option[T]] = {
    OFormat(readHandler(jsPath), writeHandler(jsPath))
  }

  final def formatHandlerWithDefault[T](jsPath: JsPath, defaultValue: => Option[T])(implicit
      format: Format[T]
  ): OFormat[Option[T]] = {
    OFormat(readHandlerWithDefault(jsPath, defaultValue), writeHandler(jsPath))
  }
}

/** OptionHandlers companion */
object OptionHandlers {

  /**
   * Default Option Handlers
   * Uses readNullable and writesNullable
   */
  object Default extends OptionHandlers {
    def readHandler[T](jsPath: JsPath)(implicit r: Reads[T]): Reads[Option[T]]          = jsPath.readNullable
    def writeHandler[T](jsPath: JsPath)(implicit writes: Writes[T]): OWrites[Option[T]] = jsPath.writeNullable
  }

  /**
   * Option Handlers to write JsNull when handling None
   * Uses readNullable and writeOptionWithNull
   */
  object WritesNull extends OptionHandlers {
    def readHandler[T](jsPath: JsPath)(implicit reads: Reads[T]): Reads[Option[T]]      = jsPath.readNullable
    def writeHandler[T](jsPath: JsPath)(implicit writes: Writes[T]): OWrites[Option[T]] = jsPath.writeOptionWithNull
  }
}
