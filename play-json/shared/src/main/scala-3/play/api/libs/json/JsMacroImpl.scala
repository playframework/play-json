/*
 * Copyright (C) 2009-2020 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.collection.Map
import scala.compiletime._
import scala.deriving._

/**
 * Implementation for the JSON macro.
 */
object JsMacroImpl {
  inline def format[A](using m: Mirror.Of[A]): OFormat[A] = OFormat[A](reads, writes)

  inline def reads[A](using m: Mirror.Of[A]): Reads[A] = new Reads[A] { self =>
    given subject: Reads[A] = this
    def reads(js: JsValue)  = js match {
      case obj @ JsObject(_) => inline m match {
        case m: Mirror.ProductOf[A] => readElems[A](obj)(using self, m)
        case m: Mirror.SumOf[A]     => readCases[A](obj)(using self, m)
      }
      case _                 => JsError("error.expected.jsobject")
    }
  }

  inline def writes[A](using m: Mirror.Of[A]): OWrites[A] = new OWrites[A] { self =>
    given subject: OWrites[A] = this
    def writes(x: A)          = inline m match {
      case m: Mirror.ProductOf[A] => writeElems[A](x)(using self, m)
      case m: Mirror.SumOf[A]     => writeCases[A](x)(using self, m)
    }
  }

  inline def readElems[A: Reads](obj: JsObject)(using m: Mirror.ProductOf[A]): JsResult[A] = {
    inline val size = constValue[Tuple.Size[m.MirroredElemTypes]]
    readElemsL[A, m.MirroredElemLabels, m.MirroredElemTypes](obj, new Array[Any](size))(0)
  }

  inline def writeElems[A: OWrites](x: A)(using m: Mirror.ProductOf[A]): JsObject =
    writeElemsL[A, m.MirroredElemLabels, m.MirroredElemTypes](x)(0, Map.empty)

  inline def readCases[A: Reads](obj: JsObject)(using m: Mirror.SumOf[A]): JsResult[A] = {
    obj.value.get(config.discriminator) match {
      case None      => JsError(JsPath \ config.discriminator, "error.missing.path")
      case Some(tjs) => {
        val vjs = obj.value.get("_value").getOrElse(obj)
        tjs.validate[String].flatMap { dis =>
          // lampepfl/dotty#11048 discriminator may be "play.api.libs.json.MacroSpec.Simple" but in MirroredElemLabels is "Simple"
          val disName = dis.drop(dis.lastIndexOf('.') + 1)
          readCasesL[A, m.MirroredElemLabels, m.MirroredElemTypes](vjs, disName)
        }
      }
    }
  }

  inline def writeCases[A: OWrites](x: A)(using m: Mirror.SumOf[A]): JsObject =
    writeCasesL[A, m.MirroredElemLabels, m.MirroredElemTypes](x, m.ordinal(x))(0)

  inline def readElemsL[A: Reads, L <: Tuple, T <: Tuple](obj: JsObject, elems: Array[Any])(n: Int)(using m: Mirror.ProductOf[A]): JsResult[A] =
    inline (erasedValue[L], erasedValue[T]) match {
      case _: (EmptyTuple, EmptyTuple) => JsSuccess(m.fromProduct(new ArrayProduct(elems)))
      case _: (l *: ls, t *: ts)       => readElems1[A, l, t](obj) match {
        case e @ JsError(_)  => e
        case JsSuccess(x, _) =>
          elems(n) = x
          readElemsL[A, ls, ts](obj, elems)(n + 1)
      }
    }

  inline def writeElemsL[A: OWrites, L <: Tuple, T <: Tuple](x: A)(n: Int, kvs: Map[String, JsValue]): JsObject =
    inline (erasedValue[L], erasedValue[T]) match {
      case _: (EmptyTuple, EmptyTuple) => JsObject(kvs)
      case _: (l *: ls, t *: ts)       => writeElemsL[A, ls, ts](x)(n + 1, kvs ++ writeElems1[A, l, t](x, n))
    }

  inline def readCasesL[A: Reads, L <: Tuple, T <: Tuple](js: JsValue, name: String): JsResult[A] =
    inline (erasedValue[L], erasedValue[T]) match {
      case _: (l *: ls, t *: ts)       =>
        if (name == typeName[l]) summonReads[t & A].reads(js)
        else readCasesL[A, ls, ts](js, name)
      case _: (EmptyTuple, EmptyTuple) => JsError("error.invalid")
    }

  inline def writeCasesL[A: OWrites, L <: Tuple, T <: Tuple](x: A, ord: Int)(n: Int): JsObject =
    inline (erasedValue[L], erasedValue[T]) match {
      case _: (l *: ls, t *: ts)       =>
        if (ord == n) {
          val xjs = summonWrites[t].writes(x.asInstanceOf[t])
          def jso = xjs match {
            case xo @ JsObject(_) => xo
            case jsv              => JsObject(Seq("_value" -> jsv))
          }
          JsObject(Map(config.discriminator -> JsString(typeName[l]))) ++ jso
        } else writeCasesL[A, ls, ts](x, ord)(n + 1)
      case _: (EmptyTuple, EmptyTuple) => throw new MatchError(x)
    }

  inline def readElems1[A: Reads, L, T](obj: JsObject): JsResult[T] = {
    val reader = inline erasedValue[T] match {
      case _: Option[a] => config.optionHandlers.readHandler(path[L])(summonReads[a]).asInstanceOf[Reads[T]]
      case _            => path[L].read(summonReads[T])
    }
    reader.reads(obj)
  }

  inline def writeElems1[A: OWrites, L, T](x: A, n: Int): Map[String, JsValue] = {
    val value = x.asInstanceOf[Product].productElement(n).asInstanceOf[T]
    inline erasedValue[T] match {
      case _: Option[a] =>
        val writer = config.optionHandlers.writeHandler(path[L])(summonWrites[a]).asInstanceOf[OWrites[T]]
        writer.writes(value).underlying
      case _            =>
        Map((config.naming(summonLabel[L]), summonWrites[T].writes(value)))
    }
  }

  inline def path[L]: JsPath            = JsPath \ config.naming(summonLabel[L])
  inline def typeName[L]: String        = config.typeNaming(summonLabel[L])
  inline def config: JsonConfiguration  = summonInline[JsonConfiguration]
  inline def summonReads[A]: Reads[A]   = summonInline[Reads[A]]
  inline def summonWrites[A]: Writes[A] = summonInline[Writes[A]]

  inline def summonLabel[L]: String = inline erasedValue[L] match {
    case _: String =>  constValue[L].asInstanceOf[String]
  }

  final class ArrayProduct[A](elems: Array[A]) extends Product {
    def canEqual(that: Any): Boolean  = true
    def productArity: Int             = elems.size
    def productElement(idx: Int): Any = elems(idx)
  }

  // lampepfl/dotty#7000 No Mirrors for value classes
  inline def valueReads [A]: Reads [A] = boom("7000", "value classes")
  inline def valueWrites[A]: Writes[A] = boom("7000", "value classes")
  inline def valueFormat[A]: Format[A] = boom("7000", "value classes")

  // lampepfl/dotty#11049 No support in Mirror for default arguments
  inline def withOptionsReads [A: Mirror.Of]: Reads[A]   = boom("11049", "default arguments")
  inline def withOptionsWrites[A: Mirror.Of]: OWrites[A] = boom("11049", "default arguments")
  inline def withOptionsFormat[A: Mirror.Of]: OFormat[A] = boom("11049", "default arguments")

  inline def boom(issueNumber: String, name: String) = {
    inline val msg = "No support in Scala 3's type class derivation (Mirrors) for " + name
    inline val url = "https://github.com/lampepfl/dotty/issues/" + issueNumber
    error(msg + ", see " + url)
  }
}
