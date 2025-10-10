/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

resolvers ++= DefaultOptions.resolvers(snapshot = true)

addSbtPlugin("org.playframework" % "play-docs-sbt-plugin" % sys.props.getOrElse("play.version", "3.0.9"))

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.4")

addSbtPlugin("com.github.sbt" % "sbt-header" % "5.11.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.20.1")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")

addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.8")

addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.5")

addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.11.2")
