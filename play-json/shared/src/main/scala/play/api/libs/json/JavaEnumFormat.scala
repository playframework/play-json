package play.api.libs.json

/**
  * Helper for format a java enum into or from json
  *
  * public enum TestEnum {
  *
  *   TEST1,
  *   TEST2,
  *   OKAY
  *
  * }
  *
  * implicit val  TestEnumFormat = JavaEnumFormat.format[TestEnum]
  *
  * @author Sebastian Hardt (s.hardt@micromata.de)
  */
object JavaEnumFormat {
  def format[A <: Enum[A]](implicit m: scala.reflect.Manifest[A]): Format[A] = {
    new JsonJavaEnumFormat[A]
  }
}

/**
  * Format for java enums from and to json
  *
  * @param classType the classType of the enum
  * @tparam A the type of the enum
  */
class JsonJavaEnumFormat[A <: Enum[A]](implicit m: scala.reflect.Manifest[A]) extends Format[A] {
  def reads(json: JsValue) = JsSuccess(Enum.valueOf[A](m.runtimeClass.asInstanceOf[Class[A]], json.as[String]))

  def writes(enumValue: A) = JsString(enumValue.name())
}
