import interplay.ScalaVersions
import ReleaseTransformations._

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.plugin.MimaKeys.{
  binaryIssueFilters, previousArtifacts
}

resolvers ++= DefaultOptions.resolvers(snapshot = true)

scalaVersion := ScalaVersions.scala212

val specsVersion = "3.8.6"
val specsBuild = Seq(
  "specs2-core"
).map("org.specs2" %% _ % specsVersion)

val logback = "ch.qos.logback" % "logback-classic" % "1.1.8"
val jacksonVersion = "2.8.5"
val jacksons = Seq(
  "com.fasterxml.jackson.core" % "jackson-core",
  "com.fasterxml.jackson.core" % "jackson-annotations",
  "com.fasterxml.jackson.core" % "jackson-databind",
  "com.fasterxml.jackson.datatype" % "jackson-datatype-jdk8",
  "com.fasterxml.jackson.datatype" % "jackson-datatype-jsr310"
).map(_ % jacksonVersion)

def jsonDependencies(scalaVersion: String) = Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion,
  logback % Test
) ++ jacksons ++ specsBuild.map(_ % Test)

// Common settings 
import com.typesafe.sbt.SbtScalariform._
import scalariform.formatter.preferences._

val previousVersion = "2.6.0-M1" // first from this separate repo
lazy val commonSettings = mimaDefaultSettings ++ (
  SbtScalariform.scalariformSettings) ++ Seq(
    previousArtifacts := Set(
      organization.value %% moduleName.value % previousVersion),
    ScalariformKeys.preferences := ScalariformKeys.preferences.value
      .setPreference(SpacesAroundMultiImports, true)
      .setPreference(SpaceInsideParentheses, false)
      .setPreference(DanglingCloseParenthesis, Preserve)
      .setPreference(PreserveSpaceBeforeArguments, true)
      .setPreference(DoubleIndentClassDeclaration, true)
  )

lazy val root = project
  .in(file("."))
  .enablePlugins(PlayRootProject)
  .aggregate(`play-json`, `play-functional`)

lazy val `play-json` = project
  .in(file("play-json"))
  .enablePlugins(PlayLibrary)
  .settings(commonSettings)
  .settings(
    binaryIssueFilters ++= Seq(
      ProblemFilters.exclude[MissingClassProblem]("play.libs.Json")
    ),
    libraryDependencies ++= jsonDependencies(scalaVersion.value))
  .dependsOn(`play-functional`)

lazy val `play-functional` = project
  .in(file("play-functional"))
  .settings(commonSettings)
  .enablePlugins(PlayLibrary)

playBuildRepoName in ThisBuild := "play-json"

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  ReleaseStep(action = Command.process("publishSigned", _), enableCrossBuild = true),
  setNextVersion,
  commitNextVersion,
  ReleaseStep(action = Command.process("sonatypeReleaseAll", _), enableCrossBuild = true),
  pushChanges
)

lazy val checkCodeFormat = taskKey[Unit]("Check that code format is following Scalariform rules")

checkCodeFormat := {
  val exitCode = "git diff --exit-code".!
  if (exitCode != 0) {
    sys.error(
      """
        |ERROR: Scalariform check failed, see differences above.
        |To fix, format your sources using sbt scalariformFormat test:scalariformFormat before submitting a pull request.
        |Additionally, please squash your commits (eg, use git commit --amend) if you're going to update this pull request.
      """.stripMargin)
  }
}

addCommandAlias("validateCode", ";scalariformFormat;checkCodeFormat")