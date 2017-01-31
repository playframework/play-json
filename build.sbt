import interplay.ScalaVersions
import ReleaseTransformations._

import com.typesafe.tools.mima.core._, ProblemFilters._
import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.plugin.MimaKeys.{
  mimaBinaryIssueFilters, mimaPreviousArtifacts
}

resolvers ++= DefaultOptions.resolvers(snapshot = true)

scalaVersion := ScalaVersions.scala212

val specsVersion = "3.8.6"
val specsBuild = Seq(
  "specs2-core"
).map("org.specs2" %% _ % specsVersion)

val jacksonVersion = "2.8.5"
val jacksons = Seq(
  "com.fasterxml.jackson.core" % "jackson-core",
  "com.fasterxml.jackson.core" % "jackson-annotations",
  "com.fasterxml.jackson.core" % "jackson-databind",
  "com.fasterxml.jackson.datatype" % "jackson-datatype-jdk8",
  "com.fasterxml.jackson.datatype" % "jackson-datatype-jsr310"
).map(_ % jacksonVersion)

val joda = Seq(
  "joda-time" % "joda-time" % "2.9.6"
    //"org.joda" % "joda-convert" % "1.8.1")
)

def jsonDependencies(scalaVersion: String) = Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion
)

// Common settings 
import com.typesafe.sbt.SbtScalariform._
import scalariform.formatter.preferences._

val previousVersion = Def.setting[Option[String]] {
  if (scalaVersion.value startsWith "2.11") Some("2.5.12")
  else Some("2.6.0-M1")
}

lazy val commonSettings = mimaDefaultSettings ++ (
  SbtScalariform.scalariformSettings) ++ Seq(
    mimaPreviousArtifacts := previousVersion.value.map { v =>
      organization.value %% moduleName.value % v
    }.toSet,
    ScalariformKeys.preferences := ScalariformKeys.preferences.value
      .setPreference(SpacesAroundMultiImports, true)
      .setPreference(SpaceInsideParentheses, false)
      .setPreference(DanglingCloseParenthesis, Preserve)
      .setPreference(PreserveSpaceBeforeArguments, true)
      .setPreference(DoubleIndentClassDeclaration, true)
  )

lazy val root = project
  .in(file("."))
  .enablePlugins(PlayRootProject, ScalaJSPlugin)
  .aggregate(`play-jsonJS`, `play-jsonJVM`,
    `play-functionalJS`, `play-functionalJVM`)

val isNew = implicitly[ProblemFilter](
  _.ref.isInstanceOf[ReversedMissingMethodProblem])

val filtersNew = Seq(
  // Macro/compile-time
  ProblemFilters.exclude[MissingClassProblem]("play.api.libs.json.JsMacroImpl$ImplicitResolver$2$ImplicitTransformer$"),
  ProblemFilters.exclude[MissingClassProblem]("play.api.libs.json.JsMacroImpl$ImplicitResolver$2$Implicit$"),
  ProblemFilters.exclude[MissingClassProblem]("play.api.libs.json.JsMacroImpl$ImplicitResolver$2$Implicit")
)

val compatFilters = {
  val validationFilter: ProblemFilter =
    !_.ref.toString.contains("validation.ValidationError")

  Seq(
    validationFilter,
    ProblemFilters.exclude[MissingClassProblem]("play.libs.Json"),
    ProblemFilters.exclude[AbstractClassProblem]("play.api.libs.json.JsBoolean"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("play.api.libs.json.ConstraintReads.min"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("play.api.libs.json.ConstraintReads.max"),
    ProblemFilters.exclude[IncompatibleMethTypeProblem]("play.api.libs.json.Reads.min"),
    ProblemFilters.exclude[IncompatibleMethTypeProblem]("play.api.libs.json.Reads.max"),
    ProblemFilters.exclude[UpdateForwarderBodyProblem]("play.api.libs.json.DefaultWrites.traversableWrites"),

    // Was deprecated
    ProblemFilters.exclude[DirectMissingMethodProblem]("play.api.libs.json.Json.toJson"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("play.api.libs.json.Json.fromJson"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("play.api.libs.json.JsError.toFlatJson"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("play.api.libs.json.JsError.toFlatJson")
  )
}

lazy val `play-json` = crossProject.crossType(CrossType.Full)
  .in(file("play-json"))
  .enablePlugins(PlayLibrary)
  .settings(commonSettings)
  .settings(
    mimaBinaryIssueFilters ++= {
      if (scalaVersion.value startsWith "2.11") {
        compatFilters ++ filtersNew :+ isNew
      } else Seq(isNew)
    },
    libraryDependencies ++= jsonDependencies(scalaVersion.value) ++ Seq(
      "org.scalatest" %%% "scalatest" % "3.0.0" % Test,
      "org.typelevel" %% "macro-compat" % "1.1.1",
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
      compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
    )
  )
  .dependsOn(`play-functional`)

lazy val `play-jsonJVM` = `play-json`.jvm.
  settings(
    libraryDependencies ++= joda ++ jacksons ++ specsBuild.map(_ % Test) :+ (
      "ch.qos.logback" % "logback-classic" % "1.1.7" % Test
    ))

lazy val `play-jsonJS` = `play-json`.js

lazy val `play-functional` = crossProject.crossType(CrossType.Pure)
  .in(file("play-functional"))
  .settings(commonSettings)
  .enablePlugins(PlayLibrary)

lazy val `play-functionalJVM` = `play-functional`.jvm
lazy val `play-functionalJS` = `play-functional`.js

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
