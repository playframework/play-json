package play.api.libs.json;


public enum JavaTestEnum {

  TEST_1(true),

  TEST_2(false);

  public final boolean testPriv;

  JavaTestEnum(boolean testPriv) {
    this.testPriv = testPriv;
  }
}
