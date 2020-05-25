

object Dependencies {
  // scalaVersion needs to be kept in sync with travis-ci
  val Scala210 = "2.10.7"
  val Scala212 = "2.12.10"
  val Scala213 = "2.13.1"
  val ScalaVersions = Seq(Scala210, Scala212, Scala213)

  val PlayVersion = sys.props.getOrElse("play.version", sys.env.getOrElse("PLAY_VERSION", "2.8.0"))
}
