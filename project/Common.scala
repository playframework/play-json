import sbt.Keys._
import sbt._
import sbt.plugins.JvmPlugin

object Common extends AutoPlugin {
  override def trigger = allRequirements

  override def requires = JvmPlugin

  val repoName = "play-json"

  override def globalSettings =
    Seq(
      organization         := "org.playframework",
      organizationName     := "The Play Framework Project",
      organizationHomepage := Some(uri("https://playframework.com/")),
      homepage             := Some(uri(s"https://github.com/playframework/${repoName}")),
      licenses             := Seq(License.Apache2),
      scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-encoding", "utf8"),
      javacOptions ++= Seq("-encoding", "UTF-8", "-Xlint:-options"),
      developers += Developer(
        "playframework",
        "The Play Framework Contributors",
        "contact@playframework.com",
        uri("https://github.com/playframework")
      ),
      description := "Play JSON"
    )
}
