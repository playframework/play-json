/*
 * Copyright (C) 2009-2018 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json;


public enum JavaTestEnum {

  TEST_1(true),

  TEST_2(false);

  public final boolean testPriv;

  JavaTestEnum(boolean testPriv) {
    this.testPriv = testPriv;
  }
}
