/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.util.{ Try => TryResult }
import scala.util.{ Success => TrySuccess }
import scala.util.{ Failure => TryFailure }

import scala.deriving.Mirror.ProductOf

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type

// TODO: Unit tests
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

  @annotation.tailrec
  private def withElems[U <: Product](
      tupled: Expr[U],
      fields: List[(Symbol, TypeRepr, Symbol)],
      prepared: List[Tuple2[String, (Ref => Term) => Term]]
  ): Map[String, (Ref => Term) => Term] = fields match {
    case (sym, t, f) :: tail => {
      val elem = ValDef.let(
        Symbol.spliceOwner,
        s"tuple${f.name}",
        Typed(tupled.asTerm.select(f), Inferred(t))
      )

      withElems(tupled, tail, (sym.name -> elem) :: prepared)
    }

    case _ => prepared.reverse.toMap
  }

  /**
   * @param tupled the tupled term
   * @param tupleTpe the tuple type
   * @param decls the field declarations
   */
  def withFields[U <: Product](
      tupled: Expr[U],
      tupleTpe: TypeRepr,
      decls: List[(Symbol, TypeRepr)],
      debug: String => Unit
  ): Map[String, (Term => Term) => Term] = {
    val fields = decls.zipWithIndex.flatMap { case ((sym, t), i) =>
      val field = tupleTpe.typeSymbol.declaredMethod(s"_${i + 1}")

      field.map { meth =>
        debug(
          s"// Field: ${sym.owner.owner.fullName}.${sym.name}, type = ${t.typeSymbol.fullName}, annotations = [${sym.annotations.map(_.show).mkString(", ")}]"
        )

        Tuple3(sym, t, meth)
      }
    }

    withElems[U](tupled, fields, List.empty)
  }

  /**
   * @tparam T the class type
   * @tparam U the type of the product corresponding to class `T`
   * @tparam R the result type (from the field operation)
   *
   * @param tpr the type for which a `ProductOf` is provided
   * @param toProduct the function to convert the input value as product `U`
   * @param types the types of the elements (fields)
   *
   * @return The tuple type + `{ v: Term => { tuple: Ref => ... } }`
   * with `v` a term of type `tpe`, and `tuple` the product created from.
   */
  def withTuple[T, U <: Product, R: Type](
      tpr: TypeRepr,
      toProduct: Expr[T => U],
      types: List[TypeRepr]
  )(using
      Type[T],
      Type[U]
  ): Tuple2[TypeRepr, Expr[T] => (Expr[U] => Expr[R]) => Expr[R]] = {
    val unappliedTupleTpr: TypeRepr = {
      if (types.isEmpty) {
        TypeRepr.of[EmptyTuple]
      } else {
        TypeRepr.typeConstructorOf(Class.forName(s"scala.Tuple${types.size}"))
      }
    }

    val tupleTpr = unappliedTupleTpr.appliedTo(types)

    tupleTpr -> {
      (in: Expr[T]) =>
        { (f: (Expr[U] => Expr[R])) =>
          '{
            val tuple: U = ${ toProduct }($in)
            ${ f('{ tuple }) }
          }
        }
    }
  }

  /**
   * Returns the elements type for `product`.
   *
   * @param owner the type representation for `T`
   */
  def productElements[T, U <: Product](
      owner: TypeRepr,
      pof: Expr[ProductOf[T]]
  ): TryResult[List[(Symbol, TypeRepr)]] = {

    @annotation.tailrec
    def elementTypes(
        max: Int,
        tpes: List[TypeRepr],
        ts: List[TypeRepr]
    ): List[TypeRepr] = tpes.headOption match {
      case Some(AppliedType(ty, a :: b :: Nil)) if ty <:< TypeRepr.of[*:] && max > 0 =>
        elementTypes(max - 1, b :: tpes.tail, a :: ts)

      case Some(AppliedType(ty, as)) if ty <:< TypeRepr.of[Tuple] && as.size <= max =>
        elementTypes(max - as.size, as ::: tpes.tail, ts)

      case Some(t) if t =:= TypeRepr.of[EmptyTuple] =>
        elementTypes(max, tpes.tail, ts)

      case Some(TypeBounds(t, _)) =>
        elementTypes(max, t :: tpes.tail, ts)

      case Some(t) =>
        elementTypes(max, tpes.tail, t :: ts)

      case _ =>
        ts.reverse
    }

    val ownerSym = owner.typeSymbol
    val paramss  = ownerSym.primaryConstructor.paramSymss.flatten.map { s => s.name -> s }.toMap

    def prepare(
        elmLabels: TypeRepr,
        elmTypes: TypeRepr
    ): List[(Symbol, TypeRepr)] = {
      val names =
        elementTypes(Int.MaxValue, List(elmLabels), List.empty).collect { case ConstantType(StringConstant(n)) => n }

      names
        .lazyZip(elementTypes(names.size, List(elmTypes), List.empty))
        .map { case (n, t) =>
          val csym = paramss.get(n)
          def fsym =
            Option(ownerSym.declaredField(n)).filterNot(_ == Symbol.noSymbol)

          val psym: Symbol = csym
            .orElse(fsym)
            .orElse {
              ownerSym.declaredMethod(n).headOption
            }
            .getOrElse(
              Symbol.newVal(
                ownerSym,
                n,
                t,
                Flags.EmptyFlags,
                Symbol.noSymbol
              )
            )

          psym -> t
        }
        .toList
    }

    val elements: Option[List[(Symbol, TypeRepr)]] = pof.asTerm.tpe match {
      case Refinement(
            Refinement(_, _, TypeBounds(t1 @ AppliedType(tycon1, _), _)),
            _,
            TypeBounds(t2 @ AppliedType(tycon2, _), _)
          ) if tycon1 <:< TypeRepr.of[Product] && tycon2 <:< TypeRepr.of[Product] =>
        Option(prepare(t2, t1))

      case Refinement(
            ref @ Refinement(_, _, TypeBounds(t1 @ TermRef(_, _), _)),
            _,
            TypeBounds(t2 @ TermRef(_, _), _)
          ) if {
            val emptyTupTpe = TypeRepr.of[EmptyTuple]
            (Ref.term(t1).tpe <:< emptyTupTpe && Ref.term(t2).tpe <:< emptyTupTpe)
          } =>
        None

      case pofTpe =>
        pofTpe.dealias.typeSymbol.tree match {
          case ClassDef(_, _, _, _, members) =>
            members
              .collect {
                case TypeDef(
                      n @ ("MirroredElemTypes" | "MirroredElemLabels"),
                      tt: TypeTree
                    ) if tt.tpe <:< TypeRepr.of[Product] =>
                  n -> tt.tpe
              }
              .sortBy(_._1) match {
              case (_, elmLabels) :: (_, elmTypes) :: Nil =>
                Option(prepare(elmLabels, elmTypes))

              case _ =>
                Some(List.empty[(Symbol, TypeRepr)])
            }

          case _ =>
            Some(List.empty[(Symbol, TypeRepr)])
        }
    }

    elements match {
      case Some(ls) if elements.isEmpty =>
        TryFailure(
          new IllegalArgumentException(
            s"Ill-typed ProductOf[${owner.typeSymbol.fullName}]: Fails to resolve element types and labels (bad refinement?)"
          )
        )

      case Some(ls) =>
        TrySuccess(ls)

      case _ =>
        TrySuccess(List.empty[(Symbol, TypeRepr)])
    }
  }
}
