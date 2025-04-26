/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.deriving.Mirror

import MacroSpec.*

private[json] trait MacroSpecCompat {
  object UsingAliasImplicits: // Required to support non-case class
    implicit val conv: Conversion[UsingAlias, Tuple1[OptionalInt]] =
      (v: UsingAlias) => Tuple1(v.v)

    implicit object ProductOfUsingAlias extends Mirror.Product {
      type MirroredType       = UsingAlias
      type MirroredElemTypes  = Tuple1[OptionalInt]
      type MirroredMonoType   = UsingAlias
      type MirroredLabel      = "UsingAlias"
      type MirroredElemLabels = Tuple1["v"]

      def fromProduct(p: Product): MirroredMonoType = {
        val v = p.productElement(0)

        new UsingAlias(p.productElement(0).asInstanceOf[OptionalInt])
      }
    }
  end UsingAliasImplicits
}
