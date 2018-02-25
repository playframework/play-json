package play.api.libs.json

import org.specs2.specification.core.Fragment

class PlatformWritesSpec extends org.specs2.mutable.Specification {
  "Locale" should {
    import LocaleFixtures._

    Fragment.foreach(locales zip objs) {
      case (locale, obj) =>
        s"be ${locale.toLanguageTag} and be written as JSON object" in {
          Json.toJson(locale)(Writes.localeObjectWrites) must_== obj
        }
    }

    Fragment.foreach(locales zip tags) {
      case (locale, tag) =>
        s"be ${locale.toLanguageTag} and be written as JSON string (tag)" in {
          Json.toJson(locale) must_== JsString(tag)
        }
    }
  }

}
