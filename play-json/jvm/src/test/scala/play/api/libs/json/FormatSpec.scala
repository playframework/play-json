/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import java.time.{
  Instant,
  LocalDateTime,
  LocalDate,
  LocalTime,
  OffsetTime,
  OffsetDateTime,
  ZonedDateTime,
  ZoneOffset,
  ZoneId
}

final class FormatSpec extends org.specs2.mutable.Specification {
  "JSON Format".title

  "Representation" should {
    import Format.Representation

    "be found" in {
      implicitly[Representation[ZoneId, JsString]]

      implicitly[Representation[java.util.Locale, JsString]]

      ok
    }

    "only be found from optional Implicits._" in {
      import Representation.Implicits._

      implicitly[Representation[LocalDateTime, JsString]]

      implicitly[Representation[LocalDate, JsString]]

      implicitly[Representation[LocalTime, JsString]]

      implicitly[Representation[OffsetDateTime, JsString]]

      implicitly[Representation[OffsetTime, JsString]]

      implicitly[Representation[ZonedDateTime, JsString]]

      implicitly[Representation[Instant, JsString]]

      implicitly[Representation[java.time.Duration, JsString]]

      implicitly[Representation[scala.concurrent.duration.FiniteDuration, JsString]]

      ok
    }
  }
}
