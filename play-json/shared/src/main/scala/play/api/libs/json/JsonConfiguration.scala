/*
 * Copyright (C) 2009-2017 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

/** JSON configuration */
sealed trait JsonConfiguration {
  /** Compile-time options for the JSON macros */
  type Opts <: Json.MacroOptions

  /** Naming strategy */
  def naming: JsonNaming
}

object JsonConfiguration {
  type Aux[O <: Json.MacroOptions] = JsonConfiguration { type Opts = O }

  private final class Impl[O <: Json.MacroOptions](
    val naming: JsonNaming = JsonNaming.Identity
  ) extends JsonConfiguration {
    type Opts = O
  }

  // These methods exist for binary compatibility, since Scala protected methods are public from a binary perspective.
  protected def apply(naming: JsonNaming): JsonConfiguration.Aux[Json.MacroOptions] = new Impl(naming)
  protected def default: JsonConfiguration.Aux[Json.MacroOptions] = apply()

  /**
   * @param naming the naming strategy
   */
  def apply[Opts <: Json.MacroOptions: Json.MacroOptions.Default](
    naming: JsonNaming = JsonNaming.Identity
  ): JsonConfiguration.Aux[Opts] = new Impl(naming)

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
    override val toString = "Identity"
  }

  /**
   * For each class property, use the snake case equivalent
   * to name its column (e.g. fooBar -> foo_bar).
   */
  object SnakeCase extends JsonNaming {
    def apply(property: String): String = {
      val length = property.length
      val result = new StringBuilder(length * 2)
      var resultLength = 0
      var wasPrevTranslated = false
      for (i <- 0 until length) {
        var c = property.charAt(i)
        if (i > 0 || i != '_') {
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
    def apply(property: String): String =
      if (property.length > 0)
        property.updated(0, Character.toUpperCase(property.charAt(0)))
      else
        property

    override val toString = "PascalCase"
  }

  /** Naming using a custom transformation function. */
  def apply(transformation: String => String): JsonNaming = new JsonNaming {
    def apply(property: String): String = transformation(property)
  }
}
