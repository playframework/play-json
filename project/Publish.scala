import sbt._
import Keys._

object Publish extends AutoPlugin {

  import bintray.BintrayPlugin
  import bintray.BintrayPlugin.autoImport._

  override def trigger = noTrigger

  override def requires = BintrayPlugin

  override def projectSettings =
    Seq(
      bintrayOrganization := Some("playframework"),
      bintrayRepository := (if (isSnapshot.value) "maven" else "snapshots"),
      bintrayPackage := "play-json",
      bintrayReleaseOnPublish := false,
      // maven style should only be used for libraries, not for plugins
      publishMavenStyle := true,
      bintrayPackageLabels := Seq("playframework", "JSON", "Scala.js")
    )
}
