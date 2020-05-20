/*
 * Copyright (C) Lightbend Inc. <https://www.lightbend.com>
 */

resolvers ++= DefaultOptions.resolvers(snapshot = true)
resolvers += Resolver.typesafeRepo("releases")

addSbtPlugin("org.foundweekends" % "sbt-bintray" % "0.5.6")

addSbtPlugin("com.typesafe.play" % "play-docs-sbt-plugin" % sys.props.getOrElse("play.version", "2.8.2"))

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.3.7")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.7.0")

addSbtPlugin("de.heikoseeberger" % "sbt-header" % "5.6.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.1.0")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.0")

addSbtPlugin("com.dwijnand" % "sbt-dynver" % "4.0.0")
