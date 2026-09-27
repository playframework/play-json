#!/usr/bin/env bash

echo MATRIX_SCALA=$MATRIX_SCALA

sbt -DscalaJSStage=full \
    -Dscala.version="$MATRIX_SCALA" \
    -J-XX:MinRAMPercentage=90.0 \
    -J-XX:MaxRAMPercentage=90.0 \
    'play-jsonJVM/testFull' || exit 1

case "$MATRIX_SCALA" in
  2.12.x | 2.13.x | 3.3.x)
    sbt -Dscala.version="$MATRIX_SCALA" publishLocal || exit 1
    ;;
esac

case "$MATRIX_SCALA" in
  2.12.*) echo "SKIPPING docs/test" ;;
       # ^ because Play 2.9.x does not get published for Scala 2.12.x anymore

  *) sbt -Dscala.version="$MATRIX_SCALA" docs/testFull || exit 2 ;;
esac
