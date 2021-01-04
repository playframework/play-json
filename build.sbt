/*
 * Copyright (C) 2009-2020 Lightbend Inc. <https://www.lightbend.com>
 */
import sbt._
import sbt.util._
import sbt.io.Path._

import com.typesafe.tools.mima.plugin.MimaKeys.mimaPreviousArtifacts

import sbtcrossproject.CrossPlugin.autoImport.crossProject
import sbtcrossproject.CrossType

resolvers ++= DefaultOptions.resolvers(snapshot = true)

def specs2(scalaVersion: String) = Seq(
  ("org.specs2" %% "specs2-core"  % "4.10.5" % Test).withDottyCompat(scalaVersion),
  ("org.specs2" %% "specs2-junit" % "4.10.5" % Test).withDottyCompat(scalaVersion),
)

val jacksonDatabindVersion = "2.10.5.1"
val jacksonDatabind = Seq(
  "com.fasterxml.jackson.core" % "jackson-databind" % jacksonDatabindVersion
)

val jacksonVersion = "2.10.5"
val jacksons = Seq(
  "com.fasterxml.jackson.core"     % "jackson-core",
  "com.fasterxml.jackson.core"     % "jackson-annotations",
  "com.fasterxml.jackson.datatype" % "jackson-datatype-jdk8",
  "com.fasterxml.jackson.datatype" % "jackson-datatype-jsr310"
).map(_ % jacksonVersion) ++ jacksonDatabind

val joda = Seq(
  "joda-time" % "joda-time" % "2.10.8"
)

def jsonDependencies(isDotty: Boolean, scalaVersion: String) =
  if (isDotty) Seq.empty
  else
    Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion
    )

// Common settings

// Do not check for previous JS artifacts for upgrade to Scala.js 1.0 because no sjs1 artifacts exist
def playJsonMimaSettings = Seq(
  mimaPreviousArtifacts := ((crossProjectPlatform.?.value, previousStableVersion.value) match {
    case (Some(JSPlatform), Some("2.8.1")) => Set.empty
    case (_, Some(previousVersion))        => Set(organization.value %%% moduleName.value % previousVersion)
    case _                                 => throw new Error("Unable to determine previous version")
  })
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

def scalacOpts(isDotty: Boolean) =
  Seq(
    "-target:jvm-1.8",
    "-Ywarn-unused:imports",
    "-Xlint:nullary-unit",
    "-Xlint",
    "-Ywarn-dead-code",
    "-Ywarn-macros:after",
    "-language:postfixOps"
  ) ++ (if (isDotty) Seq("-source:3.0-migration") else Seq.empty)

// Customise sbt-dynver's behaviour to make it work with tags which aren't v-prefixed
dynverVTagPrefix in ThisBuild := false

// Sanity-check: assert that version comes from a tag (e.g. not a too-shallow clone)
// https://github.com/dwijnand/sbt-dynver/#sanity-checking-the-version
Global / onLoad := (Global / onLoad).value.andThen { s =>
  val v = version.value
  if (dynverGitDescribeOutput.value.hasNoTags)
    throw new MessageOnlyException(
      s"Failed to derive version from git tags. Maybe run `git fetch --unshallow`? Version: $v"
    )
  s
}

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
  headerLicense := Some(HeaderLicense.Custom(s"Copyright (C) 2009-2020 Lightbend Inc. <https://www.lightbend.com>")),
  scalaVersion := Dependencies.Scala3,
  crossScalaVersions := Seq(Dependencies.Scala212, Dependencies.Scala213, Dependencies.Scala3),
  javacOptions in Compile ++= javacSettings,
  javacOptions in Test ++= javacSettings,
  javacOptions in (Compile, compile) ++= Seq("-target", "1.8"), // sbt #1785, avoids passing to javadoc
  scalacOptions ++= scalacOpts(isDotty.value),
  scalacOptions in (Compile, doc) ++= Seq(
    // Work around 2.12 bug which prevents javadoc in nested java classes from compiling.
    "-no-java-comments",
  ), {
    val silencerVersion = "1.7.1"

    libraryDependencies ++= (if (isDotty.value) {
                               Seq.empty
                             } else
                               Seq(
                                 compilerPlugin(
                                   ("com.github.ghik" % "silencer-plugin" % silencerVersion).cross(CrossVersion.full)
                                 ),
                                 ("com.github.ghik" % "silencer-lib" % silencerVersion % Provided)
                                   .cross(CrossVersion.full)
                               ))
  }
)

lazy val root = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .disablePlugins(MimaPlugin)
  .aggregate(
    `play-jsonJS`,
    `play-jsonJVM`,
    `play-functionalJS`,
    `play-functionalJVM`,
    `play-json-joda`
  )
  .settings(commonSettings)
  .settings(publish / skip := true)

lazy val `play-json` = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("play-json"))
  .enablePlugins(Omnidoc, Publish, Playdoc)
  .configs(Docs)
  .settings(
    commonSettings ++ playJsonMimaSettings ++ Seq(
      libraryDependencies ++= jsonDependencies(isDotty.value, scalaVersion.value) ++ Seq(
        "org.scalatest"     %%% "scalatest"       % "3.2.3"   % Test,
        "org.scalatestplus" %%% "scalacheck-1-15" % "3.2.3.0" % Test,
        "org.scalacheck"    %%% "scalacheck"      % "1.15.2"  % Test,
        ("com.chuusai" %% "shapeless" % "2.3.3" % Test).withDottyCompat(scalaVersion.value),
        //"org.scala-lang"    % "scala-compiler"    % scalaVersion.value % "provided"
      ),
      libraryDependencies ++=
        (CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, 13)) | Some((3, _)) => Seq()
          case _                            => Seq(compilerPlugin(("org.scalamacros" % "paradise" % "2.1.1").cross(CrossVersion.full)))
        }),
      unmanagedSourceDirectories in Compile ++= {
        //val sourceDir = (sourceDirectory in Compile).value
        // ^ gives jvm/src/main, for some reason
        val sourceDir = baseDirectory.value.getParentFile / "shared/src/main"
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((3, n))            => Seq(sourceDir / "scala-2.13+", sourceDir / "scala-3")
          case Some((2, n)) if n >= 13 => Seq(sourceDir / "scala-2.13+", sourceDir / "scala-2")
          case _                       => Seq(sourceDir / "scala-2.13-", sourceDir / "scala-2")
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

lazy val `play-jsonJS` = `play-json`.js.settings(
  scalaVersion := Dependencies.Scala213,
  crossScalaVersions -= Dependencies.Scala3
)

lazy val `play-jsonJVM` = `play-json`.jvm.settings(
  libraryDependencies ++=
    jacksons ++ specs2(scalaVersion.value) :+ (
      "ch.qos.logback" % "logback-classic" % "1.2.3" % Test
    ),
  unmanagedSourceDirectories in Test ++= (if (isDotty.value)
                                            (baseDirectory.value.getParentFile.getParentFile / "docs/manual/working/scalaGuide" ** "codeDotty").get
                                          else
                                            (baseDirectory.value.getParentFile.getParentFile / "docs/manual/working/scalaGuide" ** "code").get)
)

lazy val `play-json-joda` = project
  .in(file("play-json-joda"))
  .enablePlugins(Omnidoc, Publish)
  .settings(
    commonSettings ++ playJsonMimaSettings ++ Seq(
      libraryDependencies ++= joda ++ specs2(scalaVersion.value)
    )
  )
  .dependsOn(`play-jsonJVM`)

lazy val `play-functional` = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("play-functional"))
  .settings(
    commonSettings ++ playJsonMimaSettings
  )
  .enablePlugins(Omnidoc, Publish)

lazy val `play-functionalJVM` = `play-functional`.jvm
lazy val `play-functionalJS`  = `play-functional`.js

lazy val benchmarks = project
  .in(file("benchmarks"))
  .enablePlugins(JmhPlugin)
  .disablePlugins(MimaPlugin)
  .settings(commonSettings)
  .settings(publish / skip := true)
  .dependsOn(`play-jsonJVM`)

lazy val docs = project
  .in(file("docs"))
  .enablePlugins(PlayDocsPlugin)
  .disablePlugins(MimaPlugin)
  .configs(Docs)
  .settings(
    publish / skip := true,
    libraryDependencies ++= specs2(scalaVersion.value),
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
  .settings(
    scalaVersion := Dependencies.Scala213,
    crossScalaVersions -= Dependencies.Scala3
  )
//.dependsOn(`play-jsonJVM`)

addCommandAlias("validateCode", ";headerCheck;test:headerCheck;+scalafmtCheckAll;scalafmtSbtCheck")
