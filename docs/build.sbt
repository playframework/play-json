/*
 * Copyright (C) 2009-2019 Lightbend Inc. <https://www.lightbend.com>
 */

lazy val docs = project
  .in(file("."))
  .enablePlugins(PlayDocsPlugin)
  .settings(
    scalaVersion := "2.12.8",
    libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.0-SNAPSHOT"
  )
