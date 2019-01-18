package play.api.libs.json.json4s

import scala.language.implicitConversions

import org.json4s.{
  JArray,
  JBool,
  JDecimal,
  JDouble,
  JField,
  JInt,
  JLong,
  JNothing,
  JNull,
  JObject,
  JSet,
  JString,
  JValue
}

import play.api.libs.json.{
  JsArray,
  JsBoolean,
  JsFalse,
  JsNumber,
  JsNull,
  JsObject,
  JsString,
  JsTrue,
  JsValue
}

/**
 * {{{
 * import play.api.libs.json.json4s.JValueConverters
 *
 * val jsTrue: JsBoolean = JBool.True
 * val jbool: JBool = JsTrue
 * }}}
 */
object JValueConverters extends LowPriorityJValueImplicits {
  implicit val jnull2JsNull: JNull.type => JsNull.type = _ => JsNull

  implicit val jsnull2JNull: JsNull.type => JNull.type = _ => JNull

  implicit val jnothing2JsNull: JNothing.type => JsNull.type = _ => JsNull

  implicit def jbool2JsBoolean(jb: JBool): JsBoolean = jb match {
    case JBool.True => JsTrue
    case _ => JsFalse
  }

  implicit val jstrue2JBool: JsTrue.type => JBool = _ => JBool.True

  implicit val jsfalse2JBool: JsFalse.type => JBool = _ => JBool.False

  implicit def jsbool2JBool(jb: JsBoolean): JBool = jb match {
    case JsTrue => JBool.True
    case _ => JBool.False
  }

  implicit def jstring2JsString(js: JString): JsString = JsString(js.s)

  implicit def jsstring2Jtring(js: JsString): JString = JString(js.value)

  implicit def jdecimal2JsNumber(jd: JDecimal): JsNumber = JsNumber(jd.num)

  implicit def jsnumber2JDecimal(jn: JsNumber): JDecimal = JDecimal(jn.value)

  implicit def jint2JsNumber(ji: JInt): JsNumber = JsNumber(BigDecimal(ji.num))

  implicit def jdouble2JsNumber(ji: JDouble): JsNumber =
    JsNumber(BigDecimal(ji.num))

  implicit def jlong2JsNumber(ji: JLong): JsNumber =
    JsNumber(BigDecimal(ji.num))

  implicit def jfield2Field(jf: JField): (String, JsValue) =
    jf._1 -> jvalue2JsValue(jf._2)

  implicit def field2JField(jf: (String, JsValue)): JField =
    jf._1 -> jsvalue2JValue(jf._2)

  implicit def jobject2JsObject(jo: JObject): JsObject =
    JsObject(jo.obj.map({
      case (nme, jv) => nme -> jvalue2JsValue(jv)
    })(scala.collection.breakOut))

  implicit def jsobject2JObject(jo: JsObject): JObject =
    JObject(jo.fields.toList.map(field2JField))

  implicit def jarray2JsArray(ja: JArray): JsArray =
    JsArray(ja.arr.map(jvalue2JsValue))

  implicit def jsarray2JArray(ja: JsArray): JArray =
    JArray(ja.value.toList.map(jsvalue2JValue))

  implicit def jset2JsArray(js: JSet): JsArray =
    JsArray(js.set.toList.map(jvalue2JsValue))
}

private[json4s] sealed trait LowPriorityJValueImplicits {
  _: JValueConverters.type =>

  implicit def jvalue2JsValue(jv: JValue): JsValue = jv match {
    case a @ JArray(_) => jarray2JsArray(a)
    case JDouble(d) => JsNumber(BigDecimal(d))
    case JInt(i) => JsNumber(BigDecimal(i))
    case JLong(l) => JsNumber(BigDecimal(l))
    case JNull => JsNull
    case JNothing => JsNull
    case JBool.True => JsTrue
    case JBool.False => JsFalse
    case JBool(b) => JsBoolean(b)
    case s @ JSet(_) => jset2JsArray(s)
    case JString(s) => JsString(s)
    case JDecimal(n) => JsNumber(n)
    case o @ JObject(fs) => jobject2JsObject(o)
  }

  implicit def jsvalue2JValue(jv: JsValue): JValue = jv match {
    case a @ JsArray(_) => jsarray2JArray(a)
    case JsNull => JNull
    case JsTrue => JBool.True
    case JsFalse => JBool.False
    case JsString(s) => JString(s)
    case JsNumber(n) => JDecimal(n)
    case o @ JsObject(_) => jsobject2JObject(o)
  }
}
