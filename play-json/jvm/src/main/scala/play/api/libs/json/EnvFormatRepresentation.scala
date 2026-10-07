/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import java.util.Locale

import java.time.{
  Instant,
  LocalDate,
  LocalDateTime,
  LocalTime,
  OffsetDateTime,
  OffsetTime,
  ZoneId,
  ZonedDateTime,
  Duration => JDuration
}

import scala.concurrent.duration.FiniteDuration

private[json] trait EnvFormatRepresentation { self: Format.Representation.type =>
  import Format.Representation

  implicit val zoneId: Representation[ZoneId, JsString] =
    asString[ZoneId]

  implicit val locale: Representation[Locale, JsString] =
    asString[Locale]

  // `Representation` instances that can be optionally be imported in the scope
  // according to the selected `Reads`/`Writes`.
  private[json] trait EnvRepresentations { scope: Format.Representation.Implicits.type =>
    implicit val localDateTime: Representation[LocalDateTime, JsString] =
      asString[LocalDateTime]

    implicit val localDate: Representation[LocalDate, JsString] =
      asString[LocalDate]

    implicit val localTime: Representation[LocalTime, JsString] =
      asString[LocalTime]

    implicit val offsetDateTime: Representation[OffsetDateTime, JsString] =
      asString[OffsetDateTime]

    implicit val offsetTime: Representation[OffsetTime, JsString] =
      asString[OffsetTime]

    implicit val zonedDateTime: Representation[ZonedDateTime, JsString] =
      asString[ZonedDateTime]

    implicit val instant: Representation[Instant, JsString] =
      asString[Instant]

    implicit val javaDuration: Representation[JDuration, JsString] =
      asString[JDuration]

    implicit val finiteDuration: Representation[FiniteDuration, JsString] =
      asString[FiniteDuration]
  }
}
