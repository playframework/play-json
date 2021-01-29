/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.scalatest.Tag

/**
 * Use this tag to filter tests that are unstable in Scala 2.13.
 *
 * When using non-final versions of Scala, use the full version number
 * since this is what sbt returns for `scalaBinaryVersion` value.
 *
 * This should be removed in the next releases of Scala 2.13.
 */
object UnstableInScala213 extends Tag(s"play.api.libs.json.UnstableInScala213")
