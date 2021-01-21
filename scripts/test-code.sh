#!/usr/bin/env bash

echo SCALA_VERSION=$SCALA_VERSION
sbt -DscalaJSStage=full ++$SCALA_VERSION test publishLocal || exit 1

case "$SCALA_VERSION" in
  3.*) echo "SKIPPING docs/test" ;;
       # ^ because there is no play-docs for Scala 3
       #   and we can't use play-docs_2.13 because then:
       #    [error] Modules were resolved with conflicting cross-version suffixes in ProjectRef(uri("file:/d/play-json/"), "docs"):
       #    [error]    com.typesafe.play:play-functional _2.13, _3.0.0-M3
       #    [error]    com.typesafe.play:play-json _2.13, _3.0.0-M3

  *) sbt ++$SCALA_VERSION docs/test || exit 2 ;;
esac
