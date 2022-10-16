/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */
import sbt._
import sbt.util._
import sbt.io.Path._

import com.typesafe.tools.mima.core._

import sbtcrossproject.CrossPlugin.autoImport.crossProject
import sbtcrossproject.CrossType

resolvers ++= DefaultOptions.resolvers(snapshot = true)

val isScala3 = Def.setting {
  CrossVersion.partialVersion(scalaVersion.value).exists(_._1 != 2)
}

def specs2(scalaVersion: String) =
  Seq("core", "junit").map { n =>
    ("org.specs2" %% s"specs2-$n" % "4.17.0") % Test
  }

val jacksonDatabindVersion = "2.13.4.2"
val jacksonDatabind = Seq(
  "com.fasterxml.jackson.core" % "jackson-databind" % jacksonDatabindVersion
)

val jacksonVersion = "2.13.4"
val jacksons = Seq(
  "com.fasterxml.jackson.core"     % "jackson-core",
  "com.fasterxml.jackson.core"     % "jackson-annotations",
  "com.fasterxml.jackson.datatype" % "jackson-datatype-jdk8",
  "com.fasterxml.jackson.datatype" % "jackson-datatype-jsr310"
).map(_ % jacksonVersion) ++ jacksonDatabind

val joda = Seq(
  "joda-time" % "joda-time" % "2.11.2"
)

// Common settings

// Do not check for previous JS artifacts for upgrade to Scala.js 1.0 because no sjs1 artifacts exist
def playJsonMimaSettings = Seq(
  mimaPreviousArtifacts := ((crossProjectPlatform.?.value, previousStableVersion.value) match {
    case _ if isScala3.value               => Set.empty // no releases for Scala 3 yet
    case (Some(JSPlatform), Some("2.8.1")) => Set.empty
    case (_, Some(previousVersion)) =>
      val stableVersion = if (previousVersion.startsWith("2.10.0-RC")) "2.9.2" else previousVersion
      Set(organization.value %%% moduleName.value % stableVersion)
    case _ => throw new Error("Unable to determine previous version")
  }),
  mimaBinaryIssueFilters ++= Seq(
    // MergedOWrites is private
    ProblemFilters.exclude[Problem]("play.api.libs.json.OWrites#MergedOWrites*"),
    // [error]  * in current version, classes mixing play.api.libs.json.DefaultWrites need be recompiled to wire to the new static mixin forwarder method all super calls to method enumNameWrites()play.api.libs.json.Writes
    // Despite not being `sealed` or documented, I don't think DefaultWrites was intended to be extended by users.
    ProblemFilters.exclude[NewMixinForwarderProblem]("play.api.libs.json.DefaultWrites.enumNameWrites"),
  ),
)

val javacSettings = Seq(
  "-source",
  "11",
  "-Xlint:deprecation",
  "-Xlint:unchecked",
)

val scalacOpts = Seq(
  "-language:higherKinds",
  "-release",
  "11",
  "-Ywarn-unused:imports",
  "-Xlint:nullary-unit",
  "-Xlint",
  "-Ywarn-dead-code",
  "-Ywarn-macros:after"
)

// Customise sbt-dynver's behaviour to make it work with tags which aren't v-prefixed
ThisBuild / dynverVTagPrefix := false

// Sanity-check: assert that version comes from a tag (e.g. not a too-shallow clone)
// https://github.com/dwijnand/sbt-dynver/#sanity-checking-the-version
Global / onLoad := (Global / onLoad).value.andThen { s =>
  dynverAssertTagVersion.value
  s
}

lazy val commonSettings = Def.settings(
  // Do not buffer test output
  Test / logBuffered := false,
  Test / testOptions ++= Seq(
    // Show the duration of tests
    Tests.Argument(TestFrameworks.ScalaTest, "-oD"),
    Tests.Argument(TestFrameworks.Specs2, "showtimes"),
    // Filtering tests that are not stable in Scala 2.13 yet.
    Tests.Argument(TestFrameworks.ScalaTest, "-l", "play.api.libs.json.UnstableInScala213")
  ),
  headerLicense := Some(HeaderLicense.Custom(s"Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>")),
  scalaVersion  := Dependencies.Scala213,
  crossScalaVersions := Seq(Dependencies.Scala212, Dependencies.Scala213, Dependencies.Scala3),
  Compile / javacOptions ++= javacSettings,
  Test / javacOptions ++= javacSettings,
  Compile / compile / javacOptions ++= Seq("--release", "11"), // sbt #1785, avoids passing to javadoc
  scalacOptions ++= (if (isScala3.value) Nil else scalacOpts),
  Compile / doc / scalacOptions ++= Seq(
    // Work around 2.12 bug which prevents javadoc in nested java classes from compiling.
    "-no-java-comments",
  ),
  apiURL := Some(url("https://www.playframework.com/documentation/latest/api/scala/"))
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
  .enablePlugins(Omnidoc, Playdoc)
  .configs(Docs)
  .jsSettings(
    libraryDependencies ++= Seq(
      ("org.scala-js" %%% "scalajs-java-securerandom" % "1.0.0").cross(CrossVersion.for3Use2_13),
    )
  )
  .settings(
    commonSettings ++ playJsonMimaSettings ++ Def.settings(
      libraryDependencies ++= (
        if (isScala3.value) Seq.empty
        else
          Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
      ),
      libraryDependencies ++= Seq(
        "org.scalatest"     %%% "scalatest"       % "3.2.14"   % Test,
        "org.scalatestplus" %%% "scalacheck-1-16" % "3.2.13.0" % Test,
        "org.scalacheck"    %%% "scalacheck"      % "1.17.0"   % Test,
        ("com.chuusai" %% "shapeless" % "2.3.10").cross(CrossVersion.for3Use2_13) % Test
      ),
      libraryDependencies += {
        if (isScala3.value) {
          "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % Provided
        } else {
          "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided
        }
      },
      libraryDependencies ++=
        (CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, 13)) => Seq.empty
          case Some((3, _))  => Seq.empty
          case _             => Seq(compilerPlugin(("org.scalamacros" % "paradise" % "2.1.1").cross(CrossVersion.full)))
        }),
      Compile / unmanagedSourceDirectories += {
        // val sourceDir = (sourceDirectory in Compile).value
        // ^ gives jvm/src/main, for some reason
        val sourceDir = baseDirectory.value.getParentFile / "shared/src/main"
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, n)) if n < 13 => sourceDir / "scala-2.13-"
          case _                      => sourceDir / "scala-2.13+"
        }
      },
      Compile / sourceGenerators += Def.task {
        val dir = (Compile / sourceManaged).value

        val file = dir / "Generated.scala"
        val (writes, reads) = 1
          .to(22)
          .map { i =>
            def commaSeparated(s: Int => String)   = 1.to(i).map(s).mkString(", ")
            def newlineSeparated(s: Int => String) = 1.to(i).map(s).mkString("\n        ")
            val writerTypes                        = commaSeparated(j => s"T$j: Writes")
            val readerTypes                        = commaSeparated(j => s"T$j: Reads")
            val typeTuple                          = commaSeparated(j => s"T$j")
            val written                            = commaSeparated(j => s"implicitly[Writes[T$j]].writes(x._$j)")
            val readValues                         = commaSeparated(j => s"t$j")
            val readGenerators = newlineSeparated(j => s"t$j <- implicitly[Reads[T$j]].reads(arr(${j - 1}))")

            val writes =
              s"""  implicit def Tuple${i}W[$writerTypes]: Writes[Tuple$i[$typeTuple]] = Writes[Tuple${i}[$typeTuple]](
                 |    x => JsArray(Array($written))
                 |  )""".stripMargin

            val reads =
              s"""  implicit def Tuple${i}R[$readerTypes]: Reads[Tuple$i[$typeTuple]] = Reads[Tuple${i}[$typeTuple]] {
                 |    case JsArray(arr) if arr.size == $i =>
                 |      for {
                 |        $readGenerators
                 |      } yield Tuple$i($readValues)
                 |
                 |    case _ =>
                 |      JsError(Seq(JsPath() -> Seq(JsonValidationError("Expected array of $i elements"))))
                 |  }""".stripMargin

            (writes, reads)
          }
          .unzip

        IO.write(
          file,
          s"""package play.api.libs.json
             |
             |trait GeneratedReads {
             |${reads.mkString("\n")}
             |}
             |
             |trait GeneratedWrites{
             |${writes.mkString("\n")}
             |}
             |""".stripMargin
        )
        Seq(file)
      }.taskValue
    )
  )
  .dependsOn(`play-functional`)

lazy val `play-jsonJS` = `play-json`.js

lazy val `play-jsonJVM` = `play-json`.jvm
  .settings(
    libraryDependencies ++=
      jacksons ++ {
        if (isScala3.value)
          specs2(scalaVersion.value).map(_.exclude("org.scala-lang.modules", "scala-xml_2.13"))
        else
          specs2(scalaVersion.value)
      } :+ (
        "ch.qos.logback" % "logback-classic" % "1.4.4" % Test
      ),
    Test / unmanagedSourceDirectories ++= (docsP / PlayDocsKeys.scalaManualSourceDirectories).value,
  )
  .settings(enableJol)

def enableJol = Seq(
  libraryDependencies += "org.openjdk.jol" % "jol-core" % "0.16" % Test,
  Test / javaOptions += "-Djdk.attach.allowAttachSelf",
  compileOrder := CompileOrder.JavaThenScala,
)

lazy val `play-json-joda` = project
  .in(file("play-json-joda"))
  .enablePlugins(Omnidoc)
  .settings(
    commonSettings ++ playJsonMimaSettings ++ Seq(
      libraryDependencies ++= joda ++ specs2(scalaVersion.value),
    )
  )
  .dependsOn(`play-jsonJVM`)

lazy val `play-functional` = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("play-functional"))
  .settings(
    commonSettings ++ playJsonMimaSettings
  )
  .enablePlugins(Omnidoc)

lazy val `play-functionalJVM` = `play-functional`.jvm
lazy val `play-functionalJS`  = `play-functional`.js

lazy val benchmarks = project
  .in(file("benchmarks"))
  .enablePlugins(JmhPlugin)
  .disablePlugins(MimaPlugin)
  .settings(commonSettings)
  .settings(publish / skip := true)
  .dependsOn(`play-jsonJVM`)

val docsP = LocalProject("docs")
lazy val docs = project
  .in(file("docs"))
  .enablePlugins(PlayDocsPlugin)
  .disablePlugins(MimaPlugin)
  .configs(Docs)
  .settings(
    publish / skip := true,
    libraryDependencies ++= specs2(scalaVersion.value),
    PlayDocsKeys.validateDocs := (if (isScala3.value) () else PlayDocsKeys.validateDocs.value),
    PlayDocsKeys.scalaManualSourceDirectories := {
      val base = baseDirectory.value / "manual" / "working" / "scalaGuide"
      val code = (base ** "code").get
      if (isScala3.value) code
      else code ++ (base ** "code-2").get
    },
    PlayDocsKeys.resources += {
      val apiDocs = (`play-jsonJVM` / Compile / doc).value
      // Copy the docs to a place so they have the correct api/scala prefix
      val apiDocsStage = target.value / "api-docs-stage"
      val cacheFile    = streams.value.cacheDirectory / "api-docs-stage"
      val mappings = apiDocs.allPaths.filter(!_.isDirectory).get.pair(relativeTo(apiDocs)).map { case (file, path) =>
        file -> apiDocsStage / "api" / "scala" / path
      }
      Sync.sync(CacheStore(cacheFile))(mappings)
      PlayDocsDirectoryResource(apiDocsStage)
    },
    SettingKey[Seq[File]]("migrationManualSources") := Nil
  )
  .settings(commonSettings)
  .dependsOn(`play-jsonJVM`)

addCommandAlias(
  "validateCode",
  List(
    "headerCheckAll",
    "scalafmtSbtCheck",
    "scalafmtCheckAll",
  ).mkString(";")
)
