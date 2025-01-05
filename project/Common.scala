import sbt.Keys._
import sbt._
import sbt.plugins.JvmPlugin

object Common extends AutoPlugin {
  override def trigger = allRequirements

  override def requires = JvmPlugin

  val repoName = "play-json"

  override def globalSettings =
    Seq(
      organization         := "com.github.pjfanning",
      organizationName     := "The Play Framework Project",
      organizationHomepage := Some(url("https://github.com/pjfanning")),
      homepage             := Some(url(s"https://github.com/pjfanning/${repoName}")),
      licenses             := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.html")),
      scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-encoding", "utf8"),
      javacOptions ++= Seq("-encoding", "UTF-8", "-Xlint:-options"),
      developers += Developer(
        "playframework",
        "The Play Framework Contributors",
        "contact@playframework.com",
        url("https://github.com/playframework")
      ),
      developers += Developer(
        "pjfanning",
        "PJ Fanning",
        "",
        url("https://github.com/pjfanning")
      ),
      description := "Play JSON (fork)"
    )
}
