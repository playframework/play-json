#!/usr/bin/env bash

echo SCALA_VERSION=$SCALA_VERSION
sbt -DscalaJSStage=full ++$SCALA_VERSION test publishLocal || exit 1
sbt ++$SCALA_VERSION docs/test || exit 2
