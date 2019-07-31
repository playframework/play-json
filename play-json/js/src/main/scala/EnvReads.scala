/*
 * Copyright (C) 2009-2019 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

trait EnvReads {
  // No specific reader
}

trait EnvKeyReads { _: KeyReads.type =>
  // No specific reader
}
