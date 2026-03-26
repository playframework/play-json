/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import tools.jackson.core.{ StreamReadConstraints, StreamWriteConstraints }
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JsonConfigSpec extends AnyWordSpec with Matchers {
  "JsonConfig" should {
    "fetch default nesting depth (parsing)" in {
      JsonConfig.defaultStreamReadConstraints.getMaxNestingDepth.mustEqual(StreamReadConstraints.DEFAULT_MAX_DEPTH)
    }
    "fetch default nesting depth (serializer)" in {
      JsonConfig.defaultStreamWriteConstraints.getMaxNestingDepth.mustEqual(StreamWriteConstraints.DEFAULT_MAX_DEPTH)
    }
    "override nesting depth (parsing)" in {
      System.setProperty(JsonConfig.maxNestingDepth, "200")
      try {
        JsonConfig.loadMaxNestingDepth.mustEqual(200)
      } finally {
        System.clearProperty(JsonConfig.maxNestingDepth)
      }
    }
    "override nesting depth (serializer)" in {
      System.setProperty(JsonConfig.maxSerializerNestingDepth, "300")
      try {
        JsonConfig.loadMaxSerializerNestingDepth.mustEqual(300)
      } finally {
        System.clearProperty(JsonConfig.maxSerializerNestingDepth)
      }
    }
  }
}
