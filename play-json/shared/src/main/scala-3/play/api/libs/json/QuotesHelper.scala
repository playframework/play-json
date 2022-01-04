/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type

private[json] trait QuotesHelper {
  protected type Q <: Quotes

  protected val quotes: Q

  import quotes.reflect.*

  // format: off
  private given q: Q = quotes
  // format: on

  protected final lazy val anyValTpe: TypeRepr = TypeRepr.of[AnyVal]

  /**
   * Recursively find the sub-classes of `tpr`.
   *
   * Sub-abstract types are not listed, but their own sub-types are examined;
   * e.g. for trait `Foo`
   *
   * {{{
   * sealed trait Foo
   * case class Bar(name: String) extends Foo
   * sealed trait SubFoo extends Foo
   * case class Lorem() extends SubFoo
   * }}}
   *
   * Class `Lorem` is listed through `SubFoo`,
   * but `SubFoo` itself is not returned.
   */
  final def knownSubclasses(tpr: TypeRepr): Option[List[TypeRepr]] =
    tpr.classSymbol.flatMap { cls =>
      @annotation.tailrec
      def subclasses(
          children: List[Tree],
          out: List[TypeRepr]
      ): List[TypeRepr] = {
        val childTpr = children.headOption.collect {
          case tpd: Typed =>
            tpd.tpt.tpe

          case vd: ValDef =>
            vd.tpt.tpe

          case cd: ClassDef =>
            cd.constructor.returnTpt.tpe

        }

        childTpr match {
          case Some(child) => {
            val tpeSym = child.typeSymbol

            if (
              (tpeSym.flags.is(Flags.Abstract) &&
                tpeSym.flags.is(Flags.Sealed) &&
                !(child <:< anyValTpe)) ||
              (tpeSym.flags.is(Flags.Sealed) &&
                tpeSym.flags.is(Flags.Trait))
            ) {
              // Ignore sub-trait itself, but check the sub-sub-classes
              subclasses(tpeSym.children.map(_.tree) ::: children.tail, out)
            } else {
              subclasses(children.tail, child :: out)
            }
          }

          case _ =>
            out.reverse
        }
      }

      val types = subclasses(cls.children.map(_.tree), Nil)

      if (types.isEmpty) None else Some(types)
    }
}
