/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.deriving._

import scala.quoted._

private[json] trait OptionSupport {
  type Q <: Quotes
  protected val quotes: Q

  import quotes.reflect.*

  // format: off
  private given q: Q = quotes

  protected type Opts <: Json.MacroOptions

  /* Type of compile-time options; See [[MacroOptions]] */
  protected def optsTpe: Type[Opts]
  protected final def optsTpr: TypeRepr = TypeRepr.of(using optsTpe)

  @inline protected final def hasOption[O: Type]: Boolean =
    optsTpr <:< TypeRepr.of[O]
}
