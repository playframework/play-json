/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

private[json] trait JsNumberExtractors { self: JsNumber.type =>
  private[json] object ValidShort {
    @annotation.nowarn("msg=.*(outer\\ reference|exhaustive).*")
    def unapply(number: JsNumber): Option[Short] = number match {
      case n: JsNumericInt[?] => {
        val i = n.toInt

        if (i.isValidShort) {
          Some(i.toShort)
        } else {
          None
        }
      }

      case n: JsLong => {
        if (n.underlying.isValidShort) {
          Some(n.underlying.toShort)
        } else {
          None
        }
      }

      case n: JsBigInteger => {
        if (n.underlying.isValidShort) {
          Some(n.underlying.toShort)
        } else {
          None
        }
      }

      case n: JsNumericDouble => {
        val d = n.underlying

        if (d.isValidShort) {
          Some(d.toShort)
        } else {
          None
        }
      }

      case JsNumber(v) => {
        if (v.isValidShort) {
          Some(v.toShort)
        } else {
          None
        }
      }
    }
  }

  private[json] object ValidByte {
    @annotation.nowarn("msg=.*(outer\\ reference|exhaustive).*")
    def unapply(number: JsNumber): Option[Byte] = number match {
      case n: JsNumericInt[?] => {
        val i = n.toInt

        if (i.isValidByte) {
          Some(i.toByte)
        } else {
          None
        }
      }

      case n: JsLong => {
        if (n.underlying.isValidByte) {
          Some(n.underlying.toByte)
        } else {
          None
        }
      }

      case n: JsBigInteger => {
        if (n.underlying.isValidByte) {
          Some(n.underlying.toByte)
        } else {
          None
        }
      }

      case n: JsNumericDouble => {
        val d = n.underlying

        if (d.isValidByte) {
          Some(d.toByte)
        } else {
          None
        }
      }

      case JsNumber(v) => {
        if (v.isValidByte) {
          Some(v.toByte)
        } else {
          None
        }
      }
    }
  }

  private[json] object ValidInt {
    @annotation.nowarn("msg=.*(outer\\ reference|exhaustive).*")
    def unapply(number: JsNumber): Option[Int] = number match {
      case n: JsNumericInt[?] =>
        Some(n.toInt)

      case n: JsLong => {
        if (n.underlying.isValidInt) {
          Some(n.underlying.toInt)
        } else {
          None
        }
      }

      case n: JsBigInteger => {
        if (n.underlying.isValidInt) {
          Some(n.underlying.toInt)
        } else {
          None
        }
      }

      case n: JsNumericDouble => {
        val d = n.underlying

        if (d.isValidInt) {
          Some(d.toInt)
        } else {
          None
        }
      }

      case JsNumber(v) => {
        if (v.isValidInt) {
          Some(v.toInt)
        } else {
          None
        }
      }
    }
  }

  private[json] object ValidLong {
    @annotation.nowarn("msg=.*(outer\\ reference|exhaustive).*")
    def unapply(number: JsNumber): Option[Long] = number match {
      case n: JsNumericInt[?] =>
        Some(n.toLong)

      case n: JsLong =>
        Some(n.underlying)

      case n: JsBigInteger => {
        if (n.underlying.isValidLong) {
          Some(n.underlying.toLong)
        } else {
          None
        }
      }

      case n: JsNumericDouble => {
        val d = n.underlying

        if (d.isValidInt) {
          Some(d.toLong)
        } else {
          None
        }
      }

      case JsNumber(v) => {
        if (v.isValidLong) {
          Some(v.toLong)
        } else {
          None
        }
      }
    }
  }

  private[json] object ValidFloat {
    @annotation.nowarn("msg=.*(outer\\ reference|exhaustive).*")
    def unapply(number: JsNumber): Option[Float] = number match {
      case n: JsNumericInt[?] => Some(n.toInt.toFloat)

      case n: JsLong => Some(n.underlying.toFloat)

      case n: JsBigInteger => {
        if (n.underlying.isValidFloat) {
          Some(n.underlying.toFloat)
        } else {
          None
        }
      }

      case n: JsNumericDouble => {
        val d = n.underlying
        val f = n.toFloat

        if (!f.isInfinite && !(f == 0.0F && d != 0.0D)) {
          Some(f)
        } else {
          None
        }
      }

      case JsNumber(v) =>
        Some(v.toFloat) // TODO: Check range
    }
  }

  private[json] object ValidDouble {
    @annotation.nowarn("msg=.*(outer\\ reference|exhaustive).*")
    def unapply(number: JsNumber): Option[Double] = number match {
      case n: JsNumericInt[?] => Some(n.toDouble)

      case n: JsLong => Some(n.underlying.toDouble)

      case n: JsBigInteger => {
        if (n.underlying.isValidDouble) {
          Some(n.underlying.toDouble)
        } else {
          None
        }
      }

      case n: JsNumericDouble =>
        Some(n.underlying)

      case JsNumber(v) =>
        // TODO: Check range
        Some(v.toDouble)
    }
  }
}
