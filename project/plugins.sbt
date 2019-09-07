/*
 * Copyright (C) 2009-2017 Lightbend Inc. <https://www.lightbend.com>
 */

resolvers ++= DefaultOptions.resolvers(snapshot = true)
resolvers += Resolver.typesafeRepo("releases")

addSbtPlugin("com.typesafe.play" % "interplay" % sys.props.get("interplay.version").getOrElse("2.0.8"))

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.3.7")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.6.0")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.6")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.2")

addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.8.3")

addSbtPlugin("de.heikoseeberger" % "sbt-header" % "5.2.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.28")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.6.1")
