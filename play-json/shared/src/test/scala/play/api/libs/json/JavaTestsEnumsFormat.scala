/*
 * Copyright (C) 2009-2018 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

object JavaTestsEnumsFormat {
  implicit val testsEnumsFormat = JavaEnumFormat.format[JavaTestEnum]
}

