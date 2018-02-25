package play.api.libs.json

import java.util.Locale

import org.specs2.specification.core.Fragment

class PlatformReadsSpec extends org.specs2.mutable.Specification {
  "Locale" should {
    import LocaleFixtures._

    Fragment.foreach(locales zip objs) {
      case (locale, obj) =>
        s"be ${locale.toLanguageTag} and be read as JSON object" in {
          Json.fromJson[Locale](obj)(
            Reads.localeObjectReads) mustEqual (JsSuccess(locale))
        }
    }

    Fragment.foreach(locales zip tags) {
      case (locale, tag) =>
        s"be ${locale.toLanguageTag} and be read from JSON string (tag)" in {
          Json.fromJson[Locale](JsString(tag)) must_== JsSuccess(locale)
        }
    }
  }
}
