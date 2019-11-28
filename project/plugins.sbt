/*
 * Copyright (C) 2009-2017 Lightbend Inc. <https://www.lightbend.com>
 */

resolvers ++= DefaultOptions.resolvers(snapshot = true)
resolvers += Resolver.typesafeRepo("releases")

addSbtPlugin("com.typesafe.play" % "interplay" % sys.props.get("interplay.version").getOrElse("2.1.3"))

addSbtPlugin("com.typesafe.play" % "play-docs-sbt-plugin" % sys.props.getOrElse("play.version", "2.8.0-RC2"))

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.3.7")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.6.1")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.8.1")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "2.0.0")

addSbtPlugin("de.heikoseeberger" % "sbt-header" % "5.3.1")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.31")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.6.1")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.2.1")
