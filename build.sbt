resolvers ++= DefaultOptions.resolvers(snapshot = true)

val specsVersion = "3.8.6"
val specsBuild = Seq(
  "specs2-core",
  "specs2-junit",
  "specs2-mock"
).map("org.specs2" %% _ % specsVersion)

val logback = "ch.qos.logback" % "logback-classic" % "1.1.7"
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

val commonSettings = Seq(
  scalaVersion := "2.12.1",
  crossScalaVersions := Seq("2.12.1", "2.11.8", "2.10.6")
)

lazy val root = project
  .in(file("."))
  .aggregate(`play-json`, `play-functional`)

lazy val `play-json` = project
  .in(file("play-json"))
  .settings(commonSettings)
  .settings(libraryDependencies ++= jsonDependencies(scalaVersion.value))
  .dependsOn(`play-functional`)

lazy val `play-functional` = project
  .in(file("play-functional"))
  .settings(commonSettings)

playBuildRepoName in ThisBuild := "play-json"

import ReleaseTransformations._

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
