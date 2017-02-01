/*
 * Copyright (C) 2009-2017 Lightbend Inc. <https://www.lightbend.com>
 */

lazy val docs = project
  .in(file("."))
  .enablePlugins(PlayDocsPlugin)
  .settings(
    scalaVersion := "2.12.1",
    libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.0-SNAPSHOT"
  )