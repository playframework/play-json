import sbt.Keys._
import sbt._
import sbt.io.IO

import xsbti.HashedVirtualFileRef

object Playdoc extends AutoPlugin {

  object autoImport {
    final val Docs       = config("docs")
    val playdocDirectory = settingKey[File]("Base directory of play documentation")
    val playdocPackage   = taskKey[HashedVirtualFileRef]("Package play documentation")
  }

  import autoImport._

  override def requires = sbt.plugins.JvmPlugin

  override def trigger = noTrigger

  override def projectSettings: Seq[Def.Setting[?]] = {
    Defaults.packageTaskSettings(playdocPackage, playdocPackage / mappings) ++
      Seq(
        playdocDirectory          := (ThisBuild / baseDirectory).value / "docs" / "manual",
        playdocPackage / mappings := {
          val conv: xsbti.FileConverter = fileConverter.value
          val base: File                = playdocDirectory.value

          base.allPaths.pair(IO.relativize(base.getParentFile, _)).map { case (f, s) =>
            conv.toVirtualFile(f.toPath) -> s
          }
        },
        playdocPackage / artifactClassifier := Some("playdoc"),
        playdocPackage / artifact ~= { _.withConfigurations(Vector(Docs)) }
      ) ++ addArtifact(playdocPackage / artifact, playdocPackage)
  }
}
