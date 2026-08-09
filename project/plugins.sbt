/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

resolvers ++= DefaultOptions.resolvers(snapshot = true)

resolvers += "Maven Snapshots".at("https://central.sonatype.com/repository/maven-snapshots/")

addSbtPlugin(
  "org.playframework" % "play-docs-sbt-plugin" % sys.props.getOrElse("play.version", "3.1.0-M10-173b8802-SNAPSHOT")
)

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.6")

addSbtPlugin("com.github.sbt" % "sbt-header" % "5.11.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.22.0")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.4.0")

addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.12")

addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.4.0")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.6.2")

addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.12.0")
