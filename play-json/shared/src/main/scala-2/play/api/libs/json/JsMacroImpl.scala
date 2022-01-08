/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.annotation.tailrec
import scala.reflect.macros.blackbox

/**
 * Implementation for the JSON macro.
 */
class JsMacroImpl(val c: blackbox.Context) {
  import c.universe._

  /** Only for internal purposes */
  final class Placeholder {}

  /** Only for internal purposes */
  object Placeholder {

    implicit object Format extends OFormat[Placeholder] {
      val success                                     = JsSuccess(new Placeholder())
      def reads(json: JsValue): JsResult[Placeholder] = success
      def writes(pl: Placeholder)                     = Json.obj()
    }
  }

  def withOptionsReadsImpl[A: c.WeakTypeTag]: c.Expr[Reads[A]] =
    macroImpl[A, Reads, Reads](withOptionsConfig, "read", "map", reads = true, writes = false)

  def withOptionsWritesImpl[A: c.WeakTypeTag]: c.Expr[OWrites[A]] =
    macroImpl[A, OWrites, Writes](withOptionsConfig, "write", "contramap", reads = false, writes = true)

  def withOptionsFormatImpl[A: c.WeakTypeTag]: c.Expr[OFormat[A]] =
    macroImpl[A, OFormat, Format](withOptionsConfig, "format", "inmap", reads = true, writes = true)

  def implicitConfigReadsImpl[A: c.WeakTypeTag]: c.Expr[Reads[A]] =
    macroImpl[A, Reads, Reads](implicitOptionsConfig, "read", "map", reads = true, writes = false)

  def implicitConfigValueReads[A: c.WeakTypeTag]: c.Expr[Reads[A]] =
    valueImpl[A, Reads](implicitOptionsConfig, "read")

  def implicitConfigWritesImpl[A: c.WeakTypeTag]: c.Expr[OWrites[A]] =
    macroImpl[A, OWrites, Writes](implicitOptionsConfig, "write", "contramap", reads = false, writes = true)

  def implicitConfigValueWrites[A: c.WeakTypeTag]: c.Expr[Writes[A]] =
    valueImpl[A, Writes](implicitOptionsConfig, "write")

  def implicitConfigFormatImpl[A: c.WeakTypeTag]: c.Expr[OFormat[A]] =
    macroImpl[A, OFormat, Format](implicitOptionsConfig, "format", "inmap", reads = true, writes = true)

  def implicitConfigValueFormat[A: c.WeakTypeTag]: c.Expr[Format[A]] =
    valueImpl[A, Format](implicitOptionsConfig, "format")

  // ---

  // because binary compatibility...
  @deprecated("Use implicitConfigReadsImpl or withOptionsReadsImpl", "2.6.6")
  protected def readsImpl[A: c.WeakTypeTag, O: c.WeakTypeTag]: c.Expr[Reads[A]] =
    macroImpl[A, Reads, Reads](implicitOptionsConfig, "read", "map", reads = true, writes = false)

  @deprecated("Use implicitConfigWritesImpl or withOptionsWritesImpl", "2.6.6")
  protected def writesImpl[A: c.WeakTypeTag, O: c.WeakTypeTag]: c.Expr[OWrites[A]] =
    macroImpl[A, OWrites, Writes](implicitOptionsConfig, "write", "contramap", reads = false, writes = true)

  @deprecated("Use implicitConfigFormatImpl or withOptionsFormatImpl", "2.6.6")
  protected def formatImpl[A: c.WeakTypeTag, O: c.WeakTypeTag]: c.Expr[OFormat[A]] =
    macroImpl[A, OFormat, Format](implicitOptionsConfig, "format", "inmap", reads = true, writes = true)

  private def withOptionsConfig: c.Expr[JsonConfiguration] =
    c.Expr[JsonConfiguration](c.typecheck(q"${c.prefix}.config"))

  private def implicitOptionsConfig: c.Expr[JsonConfiguration] =
    c.Expr[JsonConfiguration](c.inferImplicitValue(c.typeOf[JsonConfiguration]))

  private def valueImpl[A, M[_]](
      config: c.Expr[JsonConfiguration],
      methodName: String
  )(implicit
      atag: c.WeakTypeTag[A],
      matag: c.WeakTypeTag[M[A]]
  ): c.Expr[M[A]] = {
    def debug(msg: => String): Unit = {
      if (debugEnabled) {
        c.info(c.enclosingPosition, msg, force = false)
      }
    }

    // All these can be sort of thought as imports
    // that can then be used later in quasi quote interpolation
    val libs = q"_root_.play.api.libs"
    val json = q"$libs.json"
    val atpe = atag.tpe.dealias
    val ctor = atpe.decl(c.universe.termNames.CONSTRUCTOR).asMethod

    ctor.paramLists match {
      case List(term: TermSymbol) :: Nil => {
        def reader = q"""
            implicitly[$json.Reads[${term.info}]].map { v =>
              new ${atpe}(v)
            }
          """

        def writer = q"""{
            val fn = implicitly[_root_.play.api.libs.functional.ContravariantFunctor[$json.Writes]]
            val w = implicitly[$json.Writes[${term.info}]]
            fn.contramap[${term.info}, ${atpe}](w, _.${term.name.toTermName})
          }"""

        val tree = methodName match {
          case "read"  => reader
          case "write" => writer
          case _       => q"""$json.Format[$atpe]($reader, $writer)"""
        }

        debug(showCode(tree))

        c.Expr[M[A]](tree)
      }

      case _ =>
        c.abort(c.enclosingPosition, s"Invalid ValueClass '${atpe}': single value expected")
    }
  }

  /**
   * Generic implementation of the macro.
   *
   * The reads/writes flags are used to say whether a reads/writes is being generated (in the case format,
   * these are both true).  This is also used to determine what arguments should be passed to various
   * functions, for example, if the apply, unapply, or both should be passed to the functional builder apply
   * method.
   *
   * @param config The configuration tree
   * @param methodName The name of the method on JsPath that gets called, ie, read/write/format
   * @param mapLikeMethod The method that's used to map the type of thing being built, used in case there is
   * only one field in the case class.
   * @param reads Whether we should generate a reads.
   * @param writes Whether we should generate a writes
   * @param atag The class of the type we're generating a reads/writes/format for.
   * @param matag The class of the reads/writes/format.
   * @param natag The class of the reads/writes/format.
   */
  private def macroImpl[A, M[_], N[_]](
      config: c.Expr[JsonConfiguration],
      methodName: String,
      mapLikeMethod: String,
      reads: Boolean,
      writes: Boolean
  )(implicit
      atag: c.WeakTypeTag[A],
      matag: c.WeakTypeTag[M[A]],
      natag: c.WeakTypeTag[N[A]]
  ): c.Expr[M[A]] = {
    def debug(msg: => String): Unit = {
      if (debugEnabled) {
        c.info(c.enclosingPosition, msg, force = false)
      }
    }

    // All these can be sort of thought as imports
    // that can then be used later in quasi quote interpolation
    val libs   = q"_root_.play.api.libs"
    val json   = q"$libs.json"
    val syntax = q"$libs.functional.syntax"
    val JsPath = q"$json.JsPath"
    val unlift = q"$syntax.unlift"
    val atpe   = atag.tpe.dealias

    // ---

    // Now we find all the implicits that we need
    final case class Implicit(
        paramType: Type,
        neededImplicit: Tree,
        tpe: Type,
        selfRef: Boolean
    )

    val optTpeCtor  = typeOf[Option[_]].typeConstructor
    val forwardName = TermName(c.freshName("forward"))

    // MacroOptions
    val options = config.actualType.member(TypeName("Opts")).asType.toTypeIn(config.actualType)

    def hasOption[Flag: c.TypeTag]: Boolean = options <:< typeOf[Flag]

    /* Utility for implicit resolution - can hardly be moved outside
     * (due to dependent types).
     *
     * @param resolvedType Per each symbol of the type parameters,
     * which type is bound to
     */
    class ImplicitResolver(resolvedType: Type => Type) {
      // The placeholder type
      private val PlaceholderType: Type = typeOf[Placeholder]

      /* Refactor the input types, by replacing any type matching the `filter`,
       * by the given `replacement`.
       */
      @tailrec
      private def refactor(
          in: List[Type],
          base: TypeSymbol,
          out: List[Type],
          tail: List[(List[Type], TypeSymbol, List[Type])],
          filter: Type => Boolean,
          replacement: Type,
          altered: Boolean
      ): (Type, Boolean) = in match {
        case tpe :: ts =>
          resolvedType(tpe) match {
            case t if filter(t) =>
              refactor(ts, base, (replacement :: out), tail, filter, replacement, true)

            case TypeRef(_, sym, as) if as.nonEmpty =>
              refactor(as, sym.asType, List.empty, (ts, base, out) :: tail, filter, replacement, altered)

            case t => refactor(ts, base, (t :: out), tail, filter, replacement, altered)
          }

        case _ => {
          val tpe = appliedType(base.toTypeConstructor, out.reverse)

          tail match {
            case (x, y, more) :: ts =>
              refactor(x, y, (tpe :: more), ts, filter, replacement, altered)

            case _ => tpe -> altered
          }
        }
      }

      /**
       * Replaces any reference to the type itself by the Placeholder type.
       * @return the normalized type + whether any self reference has been found
       */
      private def normalized(subject: Type, tpe: Type): (Type, Boolean) =
        resolvedType(tpe) match {
          case t if t =:= subject => PlaceholderType -> true

          case TypeRef(_, sym, args) if args.nonEmpty =>
            refactor(args, sym.asType, List.empty, List.empty, _ =:= subject, PlaceholderType, false)

          case t => t.dealias -> false
        }

      private class ImplicitTransformer[T](
          subject: Type
      ) extends Transformer {
        /* Restores reference to the type itself when Placeholder is found. */
        private def denormalized(ptype: Type): Type = ptype match {
          case PlaceholderType => subject

          case TypeRef(_, sym, args) if args.nonEmpty =>
            refactor(args, sym.asType, List.empty, List.empty, _ == PlaceholderType, subject, false)._1

          case _ => ptype
        }

        override def transform(tree: Tree): Tree = tree match {
          case tt: TypeTree =>
            super.transform(TypeTree(denormalized(tt.tpe).dealias))

          case Select(Select(This(TypeName("JsMacroImpl")), t), sym)
              if t.toString == "Placeholder" && sym.toString == "Format" =>
            super.transform(q"$forwardName")

          case _ => super.transform(tree)
        }
      }

      def createImplicit(subject: Type, ctag: Type)(ptype: Type): Implicit = {
        val (isOpt, tpe) = ptype.dealias match {
          case t @ TypeRef(_, _, targ :: _) if t.typeConstructor <:< optTpeCtor =>
            // Option[_] needs special treatment because we need to use XXXOpt
            true -> targ.dealias
          case t =>
            false -> t
        }

        if (isOpt) { // Option special case was applied
          val it = c.inferImplicitValue(appliedType(ctag.typeConstructor, ptype), silent = true)

          if (it != EmptyTree) {
            debug(
              s"Ignoring instance of ${ctag.typeSymbol.fullName} for ${ptype} (${it.pos.source}:${it.pos.line}:${it.pos.column}); Alias for Option[$tpe] will be handled by the nullable operations."
            )
          }
        }

        val (ntpe, selfRef) = normalized(subject, tpe)
        val ptpe            = resolvedType(ntpe)

        // infers implicit
        val neededImplicitType = appliedType(ctag.typeConstructor, ptpe)
        val tx                 = new ImplicitTransformer(subject)
        val neededImplicit = if (!selfRef) {
          c.inferImplicitValue(neededImplicitType)
        } else
          c.untypecheck(
            // Reset the type attributes on the refactored tree for the implicit
            tx.transform(
              c.inferImplicitValue(neededImplicitType)
            )
          )

        Implicit(ptype.dealias, neededImplicit, tpe.dealias, selfRef)
      }
    }

    // ---

    // Utility about apply/unapply
    class CaseClass[T](tpeArgs: List[Type])(implicit
        tag: WeakTypeTag[T]
    ) {
      // Common definitions
      private val companioned     = weakTypeOf[T].typeSymbol
      private val companionObject = companioned.companion
      private val companionType   = companionObject.typeSignature

      private val unapply    = companionType.decl(TermName("unapply"))
      private val unapplySeq = companionType.decl(TermName("unapplySeq"))

      private val hasVarArgs = unapplySeq != NoSymbol

      // Returns the unapply symbol
      private lazy val effectiveUnapply: MethodSymbol =
        Seq(unapply, unapplySeq).find(_ != NoSymbol) match {
          case None =>
            c.abort(
              c.enclosingPosition,
              s"No unapply or unapplySeq function found for $companioned: $unapply / $unapplySeq"
            )

          case Some(s) => s.asMethod
        }

      private lazy val unapplyReturnTypes: Option[List[Type]] =
        effectiveUnapply.returnType match {
          case TypeRef(_, _, Nil) => {
            c.abort(
              c.enclosingPosition,
              s"Unapply of ${companioned.fullName} has no parameters. Are you using an empty case class?"
            )
          }

          case TypeRef(_, _, args) =>
            args.head match {
              case t @ TypeRef(_, _, Nil) => Some(List(t))
              case t @ TypeRef(_, _, args) =>
                import c.universe.definitions.TupleClass
                if (!TupleClass.seq.exists(t.baseType(_).ne(NoType))) Some(List(t))
                else if (t <:< typeOf[Product]) Some(args)
                else None
              case _ => None
            }

          case _ => None
        }

      /* Deep check for type compatibility */
      @tailrec
      private def conforms(types: Seq[(Type, Type)]): Boolean =
        types.headOption match {
          case Some((TypeRef(NoPrefix, a, _), TypeRef(NoPrefix, b, _))) => { // for generic parameter
            if (a.fullName != b.fullName) {
              debug(s"Type symbols are not compatible: $a != $b")

              false
            } else conforms(types.tail)
          }

          case Some((a, b)) if a.typeArgs.size != b.typeArgs.size => {
            debug(s"Type parameters are not matching: $a != $b")
            false
          }

          case Some((a, b)) if a.typeArgs.isEmpty =>
            if (a =:= b) conforms(types.tail)
            else {
              debug(s"Types are not compatible: $a != $b")
              false
            }

          case Some((a, b)) if a.baseClasses != b.baseClasses => {
            debug(s"Generic types are not compatible: $a != $b")
            false
          }

          case Some((a, b)) =>
            conforms((a.typeArgs, b.typeArgs).zipped ++: types.tail)

          case _ => true
        }

      // The apply methods for the object
      private lazy val applies: List[MethodSymbol] =
        companionType.decl(TermName("apply")) match {
          case NoSymbol =>
            c.abort(
              c.enclosingPosition,
              s"No apply function found for ${companioned.fullName}"
            )

          case s =>
            s.asTerm.alternatives.flatMap { apply =>
              val meth = apply.asMethod

              meth.paramLists match {
                case ps :: pss if ps.nonEmpty && pss.forall {
                      case p :: _ => p.isImplicit
                      case _      => false
                    } =>
                  List(meth)

                case _ => List.empty
              }
            }
        }

      /**
       * Returns whether the case class is valid:
       * has at least only "apply" with a non empty list of parameters.
       */
      @inline def validCaseClass: (Symbol, Boolean) =
        companioned -> !applies.isEmpty

      // Find an apply method that matches the unapply
      private val maybeApply: Option[MethodSymbol] = applies.collectFirst {
        case (apply: MethodSymbol) if hasVarArgs && {
              // Option[List[c.universe.Type]]
              val someApplyTypes = apply.paramLists.headOption.map(_.map(_.asTerm.typeSignature))

              val someInitApply   = someApplyTypes.map(_.init)
              val someApplyLast   = someApplyTypes.map(_.last)
              val someInitUnapply = unapplyReturnTypes.map(_.init)
              val someUnapplyLast = unapplyReturnTypes.map(_.last)
              val initsMatch      = someInitApply == someInitUnapply
              val lastMatch = (for {
                lastApply   <- someApplyLast
                lastUnapply <- someUnapplyLast
              } yield lastApply <:< lastUnapply).getOrElse(false)

              initsMatch && lastMatch
            } =>
          apply

        case (apply: MethodSymbol) if {
              val applyParams   = apply.paramLists.headOption.toList.flatMap(identity).map(_.typeSignature)
              val unapplyParams = unapplyReturnTypes.toList.flatMap(identity)

              (applyParams.size == unapplyParams.size &&
              conforms((applyParams, unapplyParams).zipped.toSeq))
            } =>
          apply
      }

      // Parameters symbols -> Function tree
      lazy val applyFunction: Option[(Tree, List[TypeSymbol], List[Symbol], List[Option[Tree]])] =
        maybeApply.flatMap { app =>
          app.paramLists.headOption.map { params =>
            val defaultValues = params.map(_.asTerm).zipWithIndex.map { case (p, i) =>
              if (!p.isParamWithDefault) None
              else {
                val getter = TermName("apply$default$" + (i + 1))
                Some(q"$companionObject.$getter")
              }
            }

            val tree = if (hasVarArgs) {
              val applyParams = params.foldLeft(List.empty[Tree]) { (l, e) =>
                l :+ Ident(TermName(e.name.encodedName.toString))
              }
              val vals = params.foldLeft(List.empty[Tree])((l, e) =>
                // Let type inference infer the type by using the empty type
                l :+ q"val ${TermName(e.name.encodedName.toString)}: ${TypeTree()}"
              )

              q"(..$vals) => $companionObject.apply(..${applyParams.init}, ${applyParams.last}: _*)"
            } else if (tpeArgs.isEmpty) {
              q"$companionObject.apply _"
            } else q"$companionObject.apply[..$tpeArgs] _"

            (tree, app.typeParams.map(_.asType), params, defaultValues)
          }
        }

      lazy val unapplyFunction: Tree = if (tpeArgs.isEmpty) {
        q"$unlift($companionObject.$effectiveUnapply)"
      } else q"$unlift($companionObject.$effectiveUnapply[..$tpeArgs])"

      @inline private def params: List[(Name, Type)] = applyFunction match {
        case Some((_, _, ps, _)) => {
          val base = if (hasVarArgs) ps.init else ps
          val defs = base.map { p => p.name -> p.typeSignature }.toList
          val end =
            if (!hasVarArgs) List.empty[(Name, Type)]
            else {
              List(ps.last.name -> unapplyReturnTypes.get.last)
            }

          defs ++ end
        }

        case _ => List.empty
      }

      def implicits(resolver: ImplicitResolver): List[(Name, Implicit)] = {
        val createImplicit = resolver.createImplicit(atpe, natag.tpe) _

        val effectiveImplicits = params.map { case (n, t) =>
          n -> createImplicit(t)
        }

        // if any implicit is missing, abort
        val missingImplicits = effectiveImplicits.collect { case (_, Implicit(t, EmptyTree /* ~= not found */, _, _)) =>
          t
        }

        if (missingImplicits.nonEmpty) {
          c.abort(
            c.enclosingPosition,
            s"No instance of ${natag.tpe.typeSymbol.fullName} is available for ${missingImplicits.map(prettyType(_)).mkString(", ")} in the implicit scope (Hint: if declared in the same file, make sure it's declared before)"
          )
        }

        effectiveImplicits
      }

      lazy val boundTypes: Map[String, Type] =
        applyFunction.fold(Map.empty[String, Type]) { case (_, tparams, _, _) =>
          tparams
            .zip(tpeArgs)
            .map { case (sym, ty) =>
              sym.fullName -> ty
            }
            .toMap
        }

      // To print the implicit types in the compiler messages
      private def prettyType(t: Type): String =
        boundTypes.getOrElse(t.typeSymbol.fullName, t).dealias match {
          case TypeRef(_, base, args) if args.nonEmpty =>
            s"""${base.asType.fullName}[${args.map(prettyType(_)).mkString(", ")}]"""

          case t => t.typeSymbol.fullName
        }
    }

    // ---

    def directKnownSubclasses: Option[List[Type]] = {
      // Workaround for SI-7046: https://issues.scala-lang.org/browse/SI-7046
      val tpeSym = atag.tpe.typeSymbol.asClass

      @tailrec
      def allSubclasses(path: List[Symbol], subclasses: Set[Type]): Set[Type] = path match {
        case (cls: ClassSymbol) :: tail if tpeSym != cls && cls.selfType.baseClasses.contains(tpeSym) => {
          if (cls.typeParams.nonEmpty)
            c.warning(c.enclosingPosition, s"cannot handle class ${cls.fullName}: type parameter not supported")
          val newSub = if (cls.typeParams.isEmpty) Set(cls.selfType) else Set.empty
          allSubclasses(tail, subclasses ++ newSub)
        }

        case (o: ModuleSymbol) :: tail
            if o.companion == NoSymbol // not a companion object
              && o.typeSignature.baseClasses.contains(tpeSym) =>
          allSubclasses(tail, subclasses ++ Set(o.typeSignature))

        case _ :: tail => allSubclasses(tail, subclasses)
        case _         => subclasses
      }

      if (tpeSym.isSealed && tpeSym.isAbstract) {
        Some(allSubclasses(tpeSym.owner.typeSignature.decls.sorted, Set.empty).toList)
      } else None
    }

    // --- Sub implementations

    val readsType  = c.typeOf[Reads[_]]
    val writesType = c.typeOf[Writes[_]]

    def macroSealedFamilyImpl(subTypes: List[Type]): c.Expr[M[A]] = {
      if (subTypes.isEmpty) {
        c.abort(c.enclosingPosition, s"Sealed trait ${atpe} is not supported: no known subclasses")
      }

      def readLambda: Tree = {
        val resolver = new ImplicitResolver({ orig: Type => orig })
        val cases = Match(
          q"dis",
          subTypes.map { t =>
            val rtpe = appliedType(readsType, List(t))
            val reader = resolver
              .createImplicit(
                atpe,
                rtpe
              )(t)
              .neededImplicit

            if (reader.isEmpty) {
              c.abort(
                c.enclosingPosition,
                s"No instance of Reads is available for ${t.typeSymbol.fullName} in the implicit scope (Hint: if declared in the same file, make sure it's declared before)"
              )
            }

            cq"name if name == $config.typeNaming(${t.typeSymbol.fullName}) => $reader.reads(vjs)"
          } :+ cq"""_ => $json.JsError("error.invalid")"""
        )

        q"""(_: $json.JsValue) match {
          case obj @ $json.JsObject(_) => obj.value.get($config.discriminator) match {
             case Some(tjs) => {
               val vjs = obj.value.get("_value").getOrElse(obj)
               tjs.validate[String].flatMap { dis => $cases }
             }

             case _ => $json.JsError($JsPath \ $config.discriminator, "error.missing.path")
          }

          case _ => $json.JsError("error.expected.jsobject")
        }"""
      }

      def writeLambda: Tree = {
        val resolver = new ImplicitResolver({ orig: Type => orig })
        val cases = Match(
          q"v",
          subTypes.map { t =>
            val wtpe = appliedType(writesType, List(t))
            val writer = resolver
              .createImplicit(
                atpe,
                wtpe
              )(t)
              .neededImplicit

            if (writer.isEmpty) {
              c.abort(
                c.enclosingPosition,
                s"No instance of Writes is available for ${t.typeSymbol.fullName} in the implicit scope (Hint: if declared in the same file, make sure it's declared before)"
              )
            }

            // ---

            cq"""x: $t => {
            val xjs = ${writer}.writes(x)
            @inline def jso = xjs match {
              case xo @ $json.JsObject(_) => xo
              case jsv => $json.JsObject(Seq("_value" -> jsv))
            }

            $json.JsObject(Map($config.discriminator -> $json.JsString($config.typeNaming(${t.typeSymbol.fullName})))) ++ jso
          }"""
          }
        )

        q"{ v: ${atpe} => $cases }"
      }

      def tree = methodName match {
        case "read"  => q"$json.Reads[${atpe}]($readLambda)"
        case "write" => q"$json.OWrites[${atpe}]($writeLambda)"
        case _       => q"$json.OFormat($readLambda, $writeLambda)"
      }

      c.Expr[M[A]](tree)
    }

    def macroCaseImpl(tpeArgs: List[Type]): c.Expr[M[A]] = {
      val utility               = new CaseClass[A](tpeArgs)
      val (companioned, isCase) = utility.validCaseClass

      if (!isCase) {
        c.abort(
          c.enclosingPosition,
          s"Type ${companioned.fullName} is not valid: must be a case class with at least one non empty list of parameter"
        )
      }

      val (applyFunction, tparams, params, defaultValues) = utility.applyFunction match {
        case Some(info) => info

        case _ =>
          c.abort(
            c.enclosingPosition,
            s"No apply function found matching unapply parameters"
          )
      }

      // ---

      // combines all reads into CanBuildX
      val resolver = new ImplicitResolver({
        import utility.boundTypes

        { orig: Type => boundTypes.getOrElse(orig.typeSymbol.fullName, orig) }
      })

      val defaultValueMap: Map[Name, Tree] =
        if (!hasOption[Json.DefaultValues]) Map.empty
        else {
          (params, defaultValues).zipped.collect { case (p, Some(dv)) =>
            p.name.encodedName -> dv
          }.toMap
        }

      val resolvedImplicits = utility.implicits(resolver)
      val canBuild = resolvedImplicits
        .map { case (name, Implicit(pt, impl, _, _)) =>
          // Equivalent to __ \ "name", but uses a naming scheme
          // of (String) => (String) to find the correct "name"
          val cn = c.Expr[String](
            q"$config.naming(${name.decodedName.toString})"
          )
          val jspathTree = q"$JsPath \ $cn"
          val isOption   = pt.typeConstructor <:< optTpeCtor

          val defaultValue = // not applicable for 'write' only
            defaultValueMap.get(name).filter(_ => methodName != "write")

          // - If we're an default value, invoke the withDefault version
          // - If we're an option with default value,
          //   invoke the WithDefault version
          (isOption, defaultValue) match {
            case (true, Some(v)) =>
              val c = TermName(s"${methodName}HandlerWithDefault")
              q"$config.optionHandlers.$c($jspathTree, $v)($impl)"

            case (true, _) =>
              val c = TermName(s"${methodName}Handler")
              q"$config.optionHandlers.$c($jspathTree)($impl)"

            case (false, Some(v)) =>
              val c = TermName(s"${methodName}WithDefault")
              q"$jspathTree.$c($v)($impl)"

            case _ =>
              q"$jspathTree.${TermName(methodName)}($impl)"
          }
        }
        .reduceLeft[Tree] { (acc, r) => q"$acc.and($r)" }

      val multiParam = params.length > 1
      // if case class has one single field, needs to use map/contramap/inmap on the Reads/Writes/Format instead of
      // canbuild.apply
      val applyOrMap = TermName(if (multiParam) "apply" else mapLikeMethod)

      // Helper function to create parameter lists for function invocations
      // based on whether this is a reads, writes or both.
      def conditionalList[T](ifReads: T, ifWrites: T): List[T] =
        (if (reads) List(ifReads) else Nil) :::
          (if (writes) List(ifWrites) else Nil)

      val syntaxImport      = if (!multiParam && !writes) q"" else q"import $syntax._"
      @inline def buildCall = q"$canBuild.$applyOrMap(..${conditionalList(applyFunction, utility.unapplyFunction)})"
      def readResult =
        if (multiParam) q"underlying.reads(obj)"
        else q"underlying.flatMap[${atpe}] { v: ${atpe} => $json.Reads.pure(f = v) }.reads(obj)"

      val canBuildCall = methodName match {
        case "read" => {
          q"""{
          val underlying = $buildCall

          $json.Reads[${atpe}] {
            case obj @ $json.JsObject(_) => $readResult
            case _ => $json.JsError("error.expected.jsobject")
          }
        }"""
        }

        case "format" => {
          q"""{
          val underlying = $buildCall
          val rfn: $json.JsValue => $json.JsResult[${atpe}] = {
            case obj @ $json.JsObject(_) => $readResult
            case _ => $json.JsError("error.expected.jsobject")
          }

          $json.OFormat[${atpe}](rfn, underlying.writes _)
        }"""
        }

        case _ => buildCall
      }

      val finalTree =
        if (!resolvedImplicits.exists(_._2.selfRef)) {
          // there is no self reference
          q"""
        $syntaxImport

        $canBuildCall
        """
        } else {
          // Has nested reference to the same type

          val forward: Tree = methodName match {
            case "read" =>
              q"$json.Reads[${atpe}](instance.reads(_))"

            case "write" =>
              q"$json.OWrites[${atpe}](instance.writes(_))"

            case _ =>
              q"$json.OFormat[${atpe}](instance.reads(_), instance.writes(_))"
          }
          val forwardCall =
            q"private val $forwardName = $forward"

          val generated = TypeName(c.freshName("Generated"))

          q"""
        final class $generated() {
          // wrap there for self reference

          $syntaxImport

          $forwardCall

          def instance: ${matag.tpe.typeSymbol}[${atpe}] = $canBuildCall
        }

        new $generated().instance
        """
        }

      debug(showCode(finalTree))

      c.Expr[M[A]](finalTree)
    }

    def caseObjectImpl: c.Expr[M[A]] = {
      def reader =
        q"""
          $json.Reads[${atpe}] {
            case obj @ $json.JsObject(_) => $json.JsSuccess(${atpe.termSymbol})
            case _ => $json.JsError("error.expected.jsobject")
          }
         """
      def writer = q"$json.OWrites[$atpe]{_ => $json.JsObject.empty }"

      val tree = methodName match {
        case "read"  => reader
        case "write" => writer
        case _       => q"""$json.OFormat[$atpe]($reader, $writer)"""
      }

      debug(showCode(tree))

      c.Expr[M[A]](tree)
    }

    // ---

    directKnownSubclasses match {
      case Some(subTypes) => macroSealedFamilyImpl(subTypes)

      case _ =>
        atpe match {
          case _: SingletonType    => caseObjectImpl
          case TypeRef(_, _, args) => macroCaseImpl(args)
          case _ =>
            c.abort(
              c.enclosingPosition,
              s"Type ${atpe.typeSymbol.fullName} is not a valid case class or an object"
            )
        }
    }
  }

  private lazy val debugEnabled =
    Option(System.getProperty("play.json.macro.debug")).filterNot(_.isEmpty).map(_.toLowerCase).exists { v =>
      "true".equals(v) || v.substring(0, 1) == "y"
    }
}
