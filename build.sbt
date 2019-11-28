/*
 * Copyright (C) 2009-2019 Lightbend Inc. <https://www.lightbend.com>
 */
import sbt._
import sbt.util._
import sbt.io.Path._

import interplay.ScalaVersions

import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.plugin.MimaKeys.mimaBinaryIssueFilters
import com.typesafe.tools.mima.plugin.MimaKeys.mimaPreviousArtifacts

import sbtcrossproject.CrossPlugin.autoImport.crossProject
import sbtcrossproject.CrossType

resolvers ++= DefaultOptions.resolvers(snapshot = true)

playBuildRepoName in ThisBuild := "play-json"
publishTo in ThisBuild := sonatypePublishToBundle.value

val specsBuild = Def.setting[Seq[ModuleID]] {
  val specsVersion = CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 10)) => "3.9.1"
    case _             => "4.8.1"
  }

  Seq(
    "org.specs2" %% "specs2-core"  % specsVersion,
    "org.specs2" %% "specs2-junit" % specsVersion,
  )
}

val jacksonDatabindVersion = "2.10.1"
val jacksonDatabind = Seq(
  "com.fasterxml.jackson.core" % "jackson-databind" % jacksonDatabindVersion
)

val jacksonVersion = "2.10.1"
val jacksons = Seq(
  "com.fasterxml.jackson.core"     % "jackson-core",
  "com.fasterxml.jackson.core"     % "jackson-annotations",
  "com.fasterxml.jackson.datatype" % "jackson-datatype-jdk8",
  "com.fasterxml.jackson.datatype" % "jackson-datatype-jsr310"
).map(_ % jacksonVersion) ++ jacksonDatabind

val joda = Seq(
  "joda-time" % "joda-time" % "2.10.5"
)

def jsonDependencies(scalaVersion: String) = Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion
)

// Common settings

val previousVersions = Def.setting[Seq[String]] {
  Nil // Seq("2.8.0-M1") // TODO: switch to a release of 2.8, when available
}

ThisBuild / mimaFailOnNoPrevious := false

def playJsonMimaSettings = mimaDefaultSettings ++ Seq(
  mimaPreviousArtifacts := previousVersions.value.map(organization.value %%% moduleName.value % _).toSet
)

// Workaround for https://github.com/scala-js/scala-js/issues/2378
// Use "sbt -DscalaJSStage=full" in .travis.yml
scalaJSStage in ThisBuild := (sys.props.get("scalaJSStage") match {
  case Some("full") => FullOptStage
  case _            => FastOptStage
})

val javacSettings = Seq(
  "-source",
  "1.8",
  "-Xlint:deprecation",
  "-Xlint:unchecked",
)

val scalacOpts = Seq(
  "-target:jvm-1.8",
  "-Ywarn-unused:imports",
  "-Xlint:nullary-unit",
  "-Xlint",
  "-Ywarn-dead-code",
  "-Ywarn-macros:after"
)

val silencerVersion = "1.4.4"

libraryDependencies in ThisBuild ++= Seq(
  compilerPlugin(("com.github.ghik" % "silencer-plugin" % silencerVersion).cross(CrossVersion.full)),
  ("com.github.ghik" % "silencer-lib" % silencerVersion % Provided).cross(CrossVersion.full)
)

lazy val commonSettings = Def.settings(
  // Do not buffer test output
  logBuffered in Test := false,
  testOptions in Test ++= Seq(
    // Show the duration of tests
    Tests.Argument(TestFrameworks.ScalaTest, "-oD"),
    Tests.Argument(TestFrameworks.Specs2, "showtimes"),
    // Filtering tests that are not stable in Scala 2.13 yet.
    Tests.Argument(TestFrameworks.ScalaTest, "-l", "play.api.libs.json.UnstableInScala213")
  ),
  headerLicense := {
    val currentYear = java.time.Year.now(java.time.Clock.systemUTC).getValue
    Some(
      HeaderLicense.Custom(
        s"Copyright (C) 2009-$currentYear Lightbend Inc. <https://www.lightbend.com>"
      )
    )
  },
  scalaVersion := ScalaVersions.scala212,
  crossScalaVersions := Seq(ScalaVersions.scala212, ScalaVersions.scala213),
  javacOptions in Compile ++= javacSettings,
  javacOptions in Test ++= javacSettings,
  javacOptions in (Compile, compile) ++= Seq("-target", "1.8"), // sbt #1785, avoids passing to javadoc
  scalacOptions ++= scalacOpts,
  scalacOptions in (Compile, doc) ++= Seq(
    // Work around 2.12 bug which prevents javadoc in nested java classes from compiling.
    "-no-java-comments",
  )
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
  )
  .settings(commonSettings)

lazy val `play-json` = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("play-json"))
  .enablePlugins(PlayLibrary, Playdoc)
  .configs(Docs)
  .settings(
    commonSettings ++ playJsonMimaSettings ++ Seq(
      mimaBinaryIssueFilters ++= Seq(),
      libraryDependencies ++= jsonDependencies(scalaVersion.value) ++ Seq(
        "org.scalatest"  %%% "scalatest"    % "3.0.8"            % Test,
        "org.scalacheck" %%% "scalacheck"   % "1.14.2"           % Test,
        "com.chuusai"    %% "shapeless"     % "2.3.3"            % Test,
        "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
      ),
      libraryDependencies ++=
        (CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, 13)) => Seq()
          case _             => Seq(compilerPlugin(("org.scalamacros" % "paradise" % "2.1.0").cross(CrossVersion.full)))
        }),
      unmanagedSourceDirectories in Compile += {
        //val sourceDir = (sourceDirectory in Compile).value
        // ^ gives jvm/src/main, for some reason
        val sourceDir = baseDirectory.value.getParentFile / "shared/src/main"
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, n)) if n >= 13 => sourceDir / "scala-2.13+"
          case _                       => sourceDir / "scala-2.13-"
        }
      },
      sourceGenerators in Compile += Def.task {
        val dir = (sourceManaged in Compile).value

        val file = dir / "Generated.scala"
        val (writes, reads) = 1
          .to(22)
          .map {
            i =>
              def commaSeparated(s: Int => String)   = 1.to(i).map(s).mkString(", ")
              def newlineSeparated(s: Int => String) = 1.to(i).map(s).mkString("\n")
              val writerTypes                        = commaSeparated(j => s"T$j: Writes")
              val readerTypes                        = commaSeparated(j => s"T$j: Reads")
              val typeTuple                          = commaSeparated(j => s"T$j")
              val written                            = commaSeparated(j => s"implicitly[Writes[T$j]].writes(x._$j)")
              val readValues                         = commaSeparated(j => s"t$j")
              val readGenerators                     = newlineSeparated(j => s"t$j <- implicitly[Reads[T$j]].reads(arr(${j - 1}))")

              (s"""
          implicit def Tuple${i}W[$writerTypes]: Writes[Tuple${i}[$typeTuple]] = Writes[Tuple${i}[$typeTuple]](
            x => JsArray(Array($written))
          )
          """, s"""
          implicit def Tuple${i}R[$readerTypes]: Reads[Tuple${i}[$typeTuple]] = Reads[Tuple${i}[$typeTuple]]{
            case JsArray(arr) if arr.size == $i =>
              for{
                $readGenerators
              } yield Tuple$i($readValues)

            case _ =>
              JsError(Seq(JsPath() -> Seq(JsonValidationError("Expected array of $i elements"))))
          }
        """)
          }
          .unzip

        IO.write(
          file,
          s"""
          package play.api.libs.json

          trait GeneratedReads {
            ${reads.mkString("\n")}
          }

          trait GeneratedWrites{
            ${writes.mkString("\n")}
          }
        """
        )
        Seq(file)
      }.taskValue
    )
  )
  .dependsOn(`play-functional`)

lazy val `play-jsonJS` = `play-json`.js

lazy val `play-jsonJVM` = `play-json`.jvm.settings(
  libraryDependencies ++=
    joda ++ // TODO: remove joda after 2.6.0
      jacksons ++ specsBuild.value.map(_ % Test) :+ (
      "ch.qos.logback" % "logback-classic" % "1.2.3" % Test
    ),
  unmanagedSourceDirectories in Test ++= (baseDirectory.value.getParentFile.getParentFile / "docs/manual/working/scalaGuide" ** "code").get
)

lazy val `play-json-joda` = project
  .in(file("play-json-joda"))
  .enablePlugins(PlayLibrary)
  .settings(
    commonSettings ++ playJsonMimaSettings ++ Seq(
      libraryDependencies ++= joda ++ specsBuild.value.map(_ % Test)
    )
  )
  .dependsOn(`play-jsonJVM`)

lazy val `play-functional` = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("play-functional"))
  .settings(
    commonSettings ++ playJsonMimaSettings ++ Seq(
      mimaBinaryIssueFilters ++= Seq()
    )
  )
  .enablePlugins(PlayLibrary)

lazy val `play-functionalJVM` = `play-functional`.jvm
lazy val `play-functionalJS`  = `play-functional`.js

lazy val benchmarks = project
  .in(file("benchmarks"))
  .enablePlugins(JmhPlugin, PlayNoPublish)
  .settings(commonSettings)
  .dependsOn(`play-jsonJVM`)

lazy val docs = project
  .in(file("docs"))
  .enablePlugins(PlayDocsPlugin, PlayNoPublish)
  .configs(Docs)
  .settings(
    libraryDependencies ++= specsBuild.value.map(_ % Test),
    PlayDocsKeys.scalaManualSourceDirectories := (baseDirectory.value / "manual" / "working" / "scalaGuide" ** "code").get,
    PlayDocsKeys.resources += {
      val apiDocs = (doc in (`play-jsonJVM`, Compile)).value
      // Copy the docs to a place so they have the correct api/scala prefix
      val apiDocsStage = target.value / "api-docs-stage"
      val cacheFile    = streams.value.cacheDirectory / "api-docs-stage"
      val mappings = apiDocs.allPaths.filter(!_.isDirectory).get.pair(relativeTo(apiDocs)).map {
        case (file, path) => file -> apiDocsStage / "api" / "scala" / path
      }
      Sync.sync(CacheStore(cacheFile))(mappings)
      PlayDocsDirectoryResource(apiDocsStage)
    },
    SettingKey[Seq[File]]("migrationManualSources") := Nil
  )
  .settings(commonSettings)
  .dependsOn(`play-jsonJVM`)

addCommandAlias("validateCode", ";headerCheck;test:headerCheck;+scalafmtCheckAll;scalafmtSbtCheck")
