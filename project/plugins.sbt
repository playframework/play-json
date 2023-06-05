/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

resolvers ++= DefaultOptions.resolvers(snapshot = true)
resolvers += Resolver.typesafeRepo("releases")

addSbtPlugin("com.typesafe.play" % "play-docs-sbt-plugin" % sys.props.getOrElse("play.version", "2.9.0-M6"))

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.4")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.2")

addSbtPlugin("de.heikoseeberger" % "sbt-header" % "5.9.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.13.1")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.1")

addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.4.12")

addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.1")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.0")

addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.5.12")
