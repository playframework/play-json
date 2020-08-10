#!/usr/bin/env bash

sbt -DscalaJSStage=full ++$TRAVIS_SCALA_VERSION test publishLocal || exit 1
sbt ++$TRAVIS_SCALA_VERSION docs/test || exit 2
