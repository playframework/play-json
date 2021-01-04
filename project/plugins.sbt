/*
 * Copyright (C) Lightbend Inc. <https://www.lightbend.com>
 */

resolvers ++= DefaultOptions.resolvers(snapshot = true)
resolvers += Resolver.typesafeRepo("releases")

addSbtPlugin("org.foundweekends" % "sbt-bintray" % "0.6.1")

addSbtPlugin("com.typesafe.play" % "play-docs-sbt-plugin" % sys.props.getOrElse("play.version", "2.8.5"))

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.0")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.8.1")

addSbtPlugin("de.heikoseeberger" % "sbt-header" % "5.6.0")

// "It is not forward binary compatible with 1.0.x: libraries compiled with 1.1.0 cannot be used with 1.0.x."
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.0.1")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.2")

addSbtPlugin("com.dwijnand" % "sbt-dynver" % "4.1.1")
