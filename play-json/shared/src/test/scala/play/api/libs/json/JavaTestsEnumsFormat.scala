package play.api.libs.json

object JavaTestsEnumsFormat {
  implicit val testsEnumsFormat = JavaEnumFormat.format[JavaTestEnum]
}

