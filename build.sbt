/*
 * Copyright (C) 2009-2017 Lightbend Inc. <https://www.lightbend.com>
 */

import interplay.ScalaVersions
import ReleaseTransformations._
import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.plugin.MimaKeys.{mimaBinaryIssueFilters, mimaPreviousArtifacts}
import sbtcrossproject.{crossProject, CrossType}

resolvers ++= DefaultOptions.resolvers(snapshot = true)

val scala213Version = "2.13.0-M3"

val specsBuild = Def.setting[Seq[ModuleID]] {
  val specsVersion = CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 10)) => "3.9.1"
    case _ => "4.0.2"
  }

  Seq("org.specs2" %% "specs2-core" % specsVersion)
}

val jacksonVersion = "2.9.5"
val jacksons = Seq(
  "com.fasterxml.jackson.core" % "jackson-core",
  "com.fasterxml.jackson.core" % "jackson-annotations",
  "com.fasterxml.jackson.core" % "jackson-databind",
  "com.fasterxml.jackson.datatype" % "jackson-datatype-jdk8",
  "com.fasterxml.jackson.datatype" % "jackson-datatype-jsr310"
).map(_ % jacksonVersion)

val joda = Seq(
  "joda-time" % "joda-time" % "2.9.9"
    //"org.joda" % "joda-convert" % "1.8.1")
)

def jsonDependencies(scalaVersion: String) = Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion
)

// Common settings
import com.typesafe.sbt.SbtScalariform, SbtScalariform.ScalariformKeys
import scalariform.formatter.preferences._

val previousVersions = Def.setting[Seq[String]] {
  scalaVersion.value.split('.')(1) match {
    case "10" => Seq("2.6.0")
    case "11" => Seq("2.6.0")
    case "12" => Seq("2.6.0")
    case _ => Nil
  }
}

def playJsonMimaSettings = mimaDefaultSettings ++ Seq(
  mimaPreviousArtifacts := previousVersions.value.map(organization.value %%% moduleName.value % _).toSet
)

// Workaround for https://github.com/scala-js/scala-js/issues/2378
// Use "sbt -DscalaJSStage=full" in .travis.yml
scalaJSStage in ThisBuild := (sys.props.get("scalaJSStage") match {
  case Some("full") => FullOptStage
  case _ => FastOptStage
})

lazy val commonSettings = SbtScalariform.projectSettings ++ Seq(
    publishTo := Some(
      if (isSnapshot.value)
        Opts.resolver.sonatypeSnapshots
      else
        Opts.resolver.sonatypeStaging
    ),
    scalariformAutoformat := true,
    headerLicense := {
      val currentYear = java.time.Year.now(java.time.Clock.systemUTC).getValue
      Some(HeaderLicense.Custom(
        s"Copyright (C) 2009-$currentYear Lightbend Inc. <https://www.lightbend.com>"
      ))
    },
    scalaVersion := ScalaVersions.scala212,
    crossScalaVersions := Seq(
      ScalaVersions.scala210, ScalaVersions.scala211, ScalaVersions.scala212, scala213Version
    ),
    ScalariformKeys.preferences := ScalariformKeys.preferences.value
      .setPreference(SpacesAroundMultiImports, true)
      .setPreference(SpaceInsideParentheses, false)
      .setPreference(DanglingCloseParenthesis, Preserve)
      .setPreference(PreserveSpaceBeforeArguments, true)
      .setPreference(DoubleIndentConstructorArguments, false)
  )

lazy val root = project
  .in(file("."))
  .enablePlugins(PlayRootProject, ScalaJSPlugin)
  .aggregate(
    `play-jsonJS`,
    `play-jsonJVM`,
    `play-functionalJS`,
    `play-functionalJVM`,
    `play-json-joda`
  ).settings(
    commonSettings,
    publishTo := None
  )

lazy val `play-json` = crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Full)
  .in(file("play-json"))
  .enablePlugins(PlayLibrary, Playdoc)
  .settings(commonSettings)
  .settings(playJsonMimaSettings)
  .settings(
    mimaBinaryIssueFilters ++= Seq(
      // AbstractFunction1 is in scala.runtime and isn't meant to be used by end users
      ProblemFilters.exclude[MissingTypesProblem]("play.api.libs.json.JsArray$"),
      ProblemFilters.exclude[MissingTypesProblem]("play.api.libs.json.JsObject$"),
      ProblemFilters.exclude[ReversedMissingMethodProblem]("play.api.libs.json.DefaultWrites.BigIntWrites"),
      ProblemFilters.exclude[ReversedMissingMethodProblem]("play.api.libs.json.DefaultWrites.BigIntegerWrites"),
      ProblemFilters.exclude[ReversedMissingMethodProblem]("play.api.libs.json.DefaultReads.BigIntReads"),
      ProblemFilters.exclude[ReversedMissingMethodProblem]("play.api.libs.json.DefaultReads.BigIntegerReads"),
      ProblemFilters.exclude[ReversedMissingMethodProblem]("play.api.libs.json.DefaultWrites.BigIntWrites"),
      ProblemFilters.exclude[ReversedMissingMethodProblem]("play.api.libs.json.DefaultWrites.BigIntegerWrites"),
      ProblemFilters.exclude[ReversedMissingMethodProblem]("play.api.libs.json.DefaultReads.BigIntReads"),
      ProblemFilters.exclude[ReversedMissingMethodProblem]("play.api.libs.json.DefaultReads.BigIntegerReads"),
      ProblemFilters.exclude[ReversedMissingMethodProblem]("play.api.libs.json.JsonConfiguration.optionHandlers"),
      ProblemFilters.exclude[ReversedMissingMethodProblem]("play.api.libs.json.JsonConfiguration.discriminator"),
      ProblemFilters.exclude[ReversedMissingMethodProblem]("play.api.libs.json.JsonConfiguration.typeNaming"),
      ProblemFilters.exclude[ReversedMissingMethodProblem]("play.api.libs.json.JsResult.recoverWith"),
      ProblemFilters.exclude[ReversedMissingMethodProblem]("play.api.libs.json.Writes.contramap"),
      ProblemFilters.exclude[ReversedMissingMethodProblem]("play.api.libs.json.OWrites.contramap")
    ),
    libraryDependencies ++= jsonDependencies(scalaVersion.value) ++ Seq(
      "org.scalatest" %%% "scalatest" % "3.0.5-M1" % Test,
      "org.scalacheck" %%% "scalacheck" % "1.13.5" % Test,
      "com.chuusai" %% "shapeless" % "2.3.3" % Test,
      "org.typelevel" %% "macro-compat" % "1.1.1",
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
      compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
    ),
    sourceGenerators in Compile += Def.task{
      val dir = (sourceManaged in Compile).value

      val file = dir / "upickle" / "Generated.scala"
      val (writes, reads) = (1 to 22).map{ i =>
        def commaSeparated(s: Int => String) = (1 to i).map(s).mkString(", ")
        def newlineSeparated(s: Int => String) = (1 to i).map(s).mkString("\n")
        val writerTypes = commaSeparated(j => s"T$j: Writes")
        val readerTypes = commaSeparated(j => s"T$j: Reads")
        val typeTuple = commaSeparated(j => s"T$j")
        val written = commaSeparated(j => s"implicitly[Writes[T$j]].writes(x._$j)")
        val readValues = commaSeparated(j => s"t$j")
        val readGenerators = newlineSeparated(j => s"t$j <- implicitly[Reads[T$j]].reads(arr(${j-1}))")
        (s"""
          implicit def Tuple${i}W[$writerTypes]: Writes[Tuple${i}[$typeTuple]] = Writes[Tuple${i}[$typeTuple]](
            x => JsArray(Array($written))
          )
          """,s"""
          implicit def Tuple${i}R[$readerTypes]: Reads[Tuple${i}[$typeTuple]] = Reads[Tuple${i}[$typeTuple]]{
            case JsArray(arr) if arr.size == $i =>
              for{
                $readGenerators
              } yield Tuple$i($readValues)

            case _ =>
              JsError(Seq(JsPath() -> Seq(JsonValidationError("Expected array of $i elements"))))
          }
        """)
      }.unzip

      IO.write(file, s"""
          package play.api.libs.json

          trait GeneratedReads {
            ${reads.mkString("\n")}
          }

          trait GeneratedWrites{
            ${writes.mkString("\n")}
          }
        """)
      Seq(file)
    }.taskValue
  )
  .dependsOn(`play-functional`)

lazy val `play-json-joda` = project
  .in(file("play-json-joda"))
  .enablePlugins(PlayLibrary)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= joda ++ specsBuild.value.map(_ % Test)
  )
  .dependsOn(`play-jsonJVM`)

lazy val `play-jsonJVM` = `play-json`.jvm.
  settings(
    libraryDependencies ++=
      joda ++ // TODO: remove joda after 2.6.0
      jacksons ++ specsBuild.value.map(_ % Test) :+ (
      "ch.qos.logback" % "logback-classic" % "1.2.3" % Test
    ),
    unmanagedSourceDirectories in Test ++= (baseDirectory.value / ".." / ".." / "docs" / "manual" / "working" / "scalaGuide" ** "code").get
  )

lazy val `play-jsonJS` = `play-json`.js

lazy val `play-functional` = crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Pure)
  .in(file("play-functional"))
  .settings(commonSettings)
  .settings(playJsonMimaSettings)
  .enablePlugins(PlayLibrary)

lazy val `play-functionalJVM` = `play-functional`.jvm
lazy val `play-functionalJS` = `play-functional`.js

lazy val benchmarks = project
  .in(file("benchmarks"))
  .enablePlugins(JmhPlugin, PlayNoPublish)
  .settings(commonSettings)
  .dependsOn(`play-jsonJVM`)

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

addCommandAlias("validateCode", ";scalariformFormat;test:scalariformFormat;headerCheck;test:headerCheck;checkCodeFormat")
