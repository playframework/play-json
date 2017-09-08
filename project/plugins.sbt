/*
 * Copyright (C) 2009-2017 Lightbend Inc. <https://www.lightbend.com>
 */

resolvers ++= DefaultOptions.resolvers(snapshot = true)
resolvers += Resolver.typesafeRepo("releases")

addSbtPlugin("com.typesafe.play" % "interplay" % sys.props.get("interplay.version").getOrElse("1.3.7"))

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.2.24")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.1.15")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "1.1")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.0")

addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.8.0")

addSbtPlugin("de.heikoseeberger" % "sbt-header" % "3.0.1")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.19")
