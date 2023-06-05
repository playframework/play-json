#!/usr/bin/env bash

echo MATRIX_SCALA=$MATRIX_SCALA
sbt -DscalaJSStage=full \
    -J-XX:MinRAMPercentage=90.0 \
    -J-XX:MaxRAMPercentage=90.0 \
    ++$MATRIX_SCALA test publishLocal || exit 1

case "$MATRIX_SCALA" in
  2.12.*) echo "SKIPPING docs/test" ;;
       # ^ because Play 2.9.x does not get published for Scala 2.12.x anymore

  *) sbt ++$MATRIX_SCALA docs/test || exit 2 ;;
esac
