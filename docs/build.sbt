/*
 * Copyright (C) 2009-2017 Lightbend Inc. <https://www.lightbend.com>
 */

lazy val docs = project
  .in(file("."))
  .enablePlugins(PlayDocsPlugin)
  .configs(Docs)
  .settings(
    scalaVersion := "2.12.7",
    libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.0-SNAPSHOT"
  )
