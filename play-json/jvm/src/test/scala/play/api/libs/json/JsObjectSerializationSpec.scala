/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.specs2.mutable._
import play.api.libs.json.Json._

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream }

class JsObjectSerializationSpec extends Specification {
  "JsObject" should {
    "serialize/deserialize correctly" in {
      val originalObj = Json.obj(
        "field1" -> 123,
        "field2" -> "abc",
        "field3" -> JsNull,
        "obj"    -> Json.obj("field1" -> 234),
        "arr" -> JsArray(
          Seq(
            JsString("abc"),
            JsNumber(123),
            JsBoolean(true),
            JsNull,
            Json.obj("field1" -> 345)
          )
        )
      )

      val bos = new ByteArrayOutputStream()
      val out = new ObjectOutputStream(bos)
      out.writeObject(originalObj)

      val bis = new ByteArrayInputStream(bos.toByteArray)
      val in  = new ObjectInputStream(bis)
      in.readObject().asInstanceOf[JsObject].must(beEqualTo(originalObj))
    }
  }
}
