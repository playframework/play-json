object Dependencies {
  // scalaVersion needs to be kept in sync with travis-ci
  val Scala212 = "2.12.13"
  val Scala213 = "2.13.5"
  // when adding RC3, don't drop RC2 unless we must; preferable, but not
  // strictly required, to support the most recent two prereleases
  val Scala3 = Seq("3.0.0-RC2")
}
