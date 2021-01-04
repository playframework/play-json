#!/usr/bin/env bash

rm /home/travis/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scalactic/scalactic_2.13/3.1.2/scalactic_2.13-3.1.2.jar || true

echo SCALA_VERSION=$SCALA_VERSION
sbt -DscalaJSStage=full ++$SCALA_VERSION test publishLocal || exit 1
sbt ++$SCALA_VERSION docs/test || exit 2
