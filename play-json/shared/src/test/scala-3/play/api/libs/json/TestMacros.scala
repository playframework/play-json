/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.deriving.Mirror.ProductOf
import scala.quoted.*

object TestMacros:

  inline def testKnownSubtypes[T]: List[String] =
    ${ testKnownSubtypesMacro[T] }

  def testKnownSubtypesMacro[T: Type](using q: Quotes): Expr[List[String]] = {
    import q.reflect.*

    val helper = new QuotesHelper {
      type Q = q.type
      val quotes = q
    }

    Expr(
      helper.knownSubclasses(TypeRepr.of[T]).toList.flatten.map(_.show)
    )
  }

  // ---

  inline def testProductElements[T]: List[String] =
    ${ testProductElementsMacro[T] }

  def testProductElementsMacro[T: Type](using q: Quotes): Expr[List[String]] = {
    import q.reflect.*

    val helper = new QuotesHelper {
      type Q = q.type
      val quotes = q
    }

    val tpe = TypeRepr.of[T]

    val names = Expr.summon[ProductOf[T]] match {
      case Some(expr) =>
        helper
          .productElements(tpe, expr)
          .map(_.map { case (sym, tpr) =>
            Tuple3(sym, sym.annotations, tpr).toString
          })
          .getOrElse(List.empty[String])

      case _ =>
        List.empty[String]
    }

    Expr(names)
  }

  // ---

  inline def testWithTuple[T <: Product](
      pure: T
  ): String =
    ${ testWithTupleMacro[T, T]('{ pure }, '{ identity[T] }) }

  inline def testWithTuple[T, P <: Product](
      pure: T
  )(using
      conv: Conversion[T, P]
  ): String =
    ${ testWithTupleMacro[T, P]('{ pure }, '{ conv(_: T) }) }

  def testWithTupleMacro[T: Type, P <: Product: Type](
      pure: Expr[T],
      toProduct: Expr[T => P]
  )(using
      q: Quotes
  ): Expr[String] = {
    import q.reflect.*

    val helper = new QuotesHelper {
      type Q = q.type
      val quotes = q
    }

    val tpe = TypeRepr.of[T]
    val tpeElements = Expr
      .summon[ProductOf[T]]
      .map {
        helper.productElements(tpe, _).get
      }
      .getOrElse(List.empty[(Symbol, TypeRepr)])

    val types = tpeElements.map(_._2)

    val (tupleTpe, withTuple) =
      helper.withTuple[T, P, String](tpe, toProduct, types)

    withTuple(pure) { (tupled: Expr[P]) =>
      val a = Expr(tupleTpe.show)

      '{
        $a + "/" + ${ tupled }.toString
      }
    }
  }

  inline def testWithFields[T <: Product](pure: T): String =
    ${ testWithFieldsMacro[T]('{ pure }) }

  def testWithFieldsMacro[T <: Product: Type](
      pure: Expr[T]
  )(using
      q: Quotes
  ): Expr[String] = {
    import q.reflect.*

    val helper = new QuotesHelper {
      type Q = q.type
      val quotes = q
    }

    val tpe = TypeRepr.of[T]
    val tpeElements = Expr
      .summon[ProductOf[T]]
      .map {
        helper.productElements(tpe, _).get
      }
      .get
    val types = tpeElements.map(_._2)

    val (tupleTpe, withTuple) =
      helper.withTuple[T, T, String](tpe, '{ identity[T] }, types)

    withTuple(pure) { (tupled: Expr[T]) =>
      val fieldMap =
        helper.withFields(tupled, tupleTpe, tpeElements, debug = _ => ())

      val strs: List[Expr[String]] = fieldMap.map { case (nme, withField) =>
        withField { fi =>
          val n = Expr[String](nme)

          '{ $n + "=" + ${ fi.asExpr }.toString }.asTerm
        }.asExprOf[String]
      }.toList

      '{ ${ Expr.ofList(strs) }.mkString(",") }
    }
  }

end TestMacros
