/*
 * Copyright (C) from 2022 The Play Framework Contributors, 2011-2021 Lightbend Inc.
 */

package play.api.libs.json

import org.openjdk.jmh.annotations._

/**
 * ==Quick Run==
 * benchmarks / Jmh / run -wi 0 -i 1 -r 10ms -f 1 .*JsonNumberBench
 *
 * ==Long Run with reporting==
 * benchmarks / Jmh / run -wi 5 -i 100 -f 3 -rf csv -rff target/jmh/numbers.csv .*JsonNumberBench'
 *
 * Measures the numeric paths independently: parsing creates a `JsNumber`,
 * materialization accesses its `BigDecimal` value, and the remaining methods
 * cover numeric `Reads`, factories, and `Writes`.
 */
@State(Scope.Benchmark)
class JsonNumberBench {
  private val smallIntegerJson = "42"
  private val shortJson        = "32767"
  private val intJson          = "2147483647"
  private val longJson         = "9223372036854775807"
  private val bigIntegerJson   = "1234567890123456789012345678901234567890"
  private val floatJson        = "12345.5"
  private val decimalJson      = "12345.6789"
  private val largeDecimalJson = "1234567890123456789012345678901234567890.12345678901234567890"

  private val byteValue: Byte          = 42
  private val shortValue: Short        = 32767
  private val intValue                 = Int.MaxValue
  private val longValue                = Long.MaxValue
  private val floatValue               = 12345.678F
  private val doubleValue              = 12345.6789D
  private val bigIntValue              = BigInt(bigIntegerJson)
  private val bigDecimalValue          = BigDecimal(decimalJson)
  private val largeBigDecimalValue     = BigDecimal(largeDecimalJson)

  // ---

  /** Parses a small integer into a `JsNumber`. */
  @Benchmark def parseSmallInteger: JsNumber = Json.parse(smallIntegerJson).asInstanceOf[JsNumber]

  /** Parses an `Int`-sized integer into a `JsNumber`. */
  @Benchmark def parseInt: JsNumber = Json.parse(intJson).asInstanceOf[JsNumber]

  /** Parses a `Long`-sized integer into a `JsNumber`. */
  @Benchmark def parseLong: JsNumber = Json.parse(longJson).asInstanceOf[JsNumber]

  /** Parses a large integer into a `JsNumber`. */
  @Benchmark def parseBigInteger: JsNumber = Json.parse(bigIntegerJson).asInstanceOf[JsNumber]

  /** Parses a decimal into a `JsNumber`. */
  @Benchmark def parseDecimal: JsNumber = Json.parse(decimalJson).asInstanceOf[JsNumber]

  /** Parses a large decimal into a `JsNumber`. */
  @Benchmark def parseLargeDecimal: JsNumber = Json.parse(largeDecimalJson).asInstanceOf[JsNumber]

  /** Parses and only materializes a small integer value. */
  @Benchmark def parseSmallIntegerValue: BigDecimal = Json.parse(smallIntegerJson).asInstanceOf[JsNumber].value

  /** Parses and only materializes an `Int`-sized integer value. */
  @Benchmark def evalParsedInt: BigDecimal =
    Json.parse(intJson).asInstanceOf[JsNumber].value

  /** Parses and only materializes a `Long`-sized integer value. */
  @Benchmark def evalParsedLong: BigDecimal =
    Json.parse(longJson).asInstanceOf[JsNumber].value

  /** Parses and only materializes a large integer value. */
  @Benchmark def evalParsedBigInteger: BigDecimal =
    Json.parse(bigIntegerJson).asInstanceOf[JsNumber].value

  /** Parses and only materializes a decimal value. */
  @Benchmark def evalParsedDecimal: BigDecimal =
    Json.parse(decimalJson).asInstanceOf[JsNumber].value

  /** Parses and only materializes a large decimal value. */
  @Benchmark def evalParsedLargeDecimal: BigDecimal =
    Json.parse(largeDecimalJson).asInstanceOf[JsNumber].value

  // ---

  /** Parses a number and reads it as a `Byte`. */
  @Benchmark def readByte: Byte = Json.parse(smallIntegerJson).as[Byte]

  /** Parses a number and reads it as a `Short`. */
  @Benchmark def readShort: Short = Json.parse(shortJson).as[Short]

  /** Parses a number and reads it as an `Int`. */
  @Benchmark def readInt: Int = Json.parse(intJson).as[Int]

  /** Parses a number and reads it as a `Long`. */
  @Benchmark def readLong: Long = Json.parse(longJson).as[Long]

  /** Parses a number and reads it as a `Float`. */
  @Benchmark def readFloat: Float = Json.parse(floatJson).as[Float]

  /** Parses a number and reads it as a `Double`. */
  @Benchmark def readDouble: Double = Json.parse(decimalJson).as[Double]

  /** Parses a number and reads it as a `BigInt`. */
  @Benchmark def readBigInt: BigInt = Json.parse(bigIntegerJson).as[BigInt]

  /** Parses a number and reads it as a `BigDecimal`. */
  @Benchmark def readBigDecimal: BigDecimal = Json.parse(largeDecimalJson).as[BigDecimal]

  // ---

  /** Creates a `JsNumber` from a `Short`. */
  @Benchmark def jsNumberShort: JsNumber = JsNumber(shortValue)

  /** Creates a `JsNumber` from an `Int`. */
  @Benchmark def jsNumberInt: JsNumber = JsNumber(intValue)

  /** Creates a `JsNumber` from a `Long`. */
  @Benchmark def jsNumberLong: JsNumber = JsNumber(longValue)

  /** Creates a `JsNumber` from a `Float`. */
  @Benchmark def jsNumberFloat: JsNumber = JsNumber(floatValue)

  /** Creates a `JsNumber` from a `Double`. */
  @Benchmark def jsNumberDouble: JsNumber = JsNumber(doubleValue)

  /** Creates a `JsNumber` from a `BigInt`. */
  @Benchmark def jsNumberBigInt: JsNumber = JsNumber(BigDecimal(bigIntValue))

  /** Creates a `JsNumber` from a `BigDecimal`. */
  @Benchmark def jsNumberBigDecimal: JsNumber = JsNumber(bigDecimalValue)

  /** Creates a `JsNumber` from a large `BigDecimal`. */
  @Benchmark def jsNumberLargeBigDecimal: JsNumber = JsNumber(largeBigDecimalValue)

  // ---

  /** Writes a `Byte` as JSON. */
  @Benchmark def writeByte: JsValue = Json.toJson(byteValue)

  /** Writes a `Short` as JSON. */
  @Benchmark def writeShort: JsValue = Json.toJson(shortValue)

  /** Writes an `Int` as JSON. */
  @Benchmark def writeInt: JsValue = Json.toJson(intValue)

  /** Writes a `Long` as JSON. */
  @Benchmark def writeLong: JsValue = Json.toJson(longValue)

  /** Writes a `Float` as JSON. */
  @Benchmark def writeFloat: JsValue = Json.toJson(floatValue)

  /** Writes a `Double` as JSON. */
  @Benchmark def writeDouble: JsValue = Json.toJson(doubleValue)

  /** Writes a `BigInt` as JSON. */
  @Benchmark def writeBigInt: JsValue = Json.toJson(bigIntValue)

  /** Writes a `BigDecimal` as JSON. */
  @Benchmark def writeBigDecimal: JsValue = Json.toJson(bigDecimalValue)

  /** Writes a large `BigDecimal` as JSON. */
  @Benchmark def writeLargeBigDecimal: JsValue = Json.toJson(largeBigDecimalValue)

  // ---

  private val eagerShortNumber: JsNumber = JsNumber(shortValue)

  /** Serializes a `JsNumber` from a `Short`. */
  @Benchmark def serializeShort: Array[Byte] = Json.toBytes(eagerShortNumber)

  private val eagerIntNumber: JsNumber = JsNumber(intValue)

  /** Serializes a `JsNumber` from an `Int`. */
  @Benchmark def serializeInt: Array[Byte] = Json.toBytes(eagerIntNumber)

  private val eagerLongNumber: JsNumber = JsNumber(longValue)

  /** Serializes a `JsNumber` from a `Long`. */
  @Benchmark def serializeLong: Array[Byte] = Json.toBytes(eagerLongNumber)

  private val eagerFloatNumber: JsNumber = JsNumber(floatValue)

  /** Serializes a `JsNumber` from a `Float`. */
  @Benchmark def serializeFloat: Array[Byte] = Json.toBytes(eagerFloatNumber)

  private val eagerDoubleNumber: JsNumber = JsNumber(doubleValue)

  /** Serializes a `JsNumber` from a `Double`. */
  @Benchmark def serializeDouble: Array[Byte] = Json.toBytes(eagerDoubleNumber)

  private val eagerBigIntNumber: JsNumber = JsNumber(BigDecimal(bigIntValue))

  /** Serializes a `JsNumber` from a `BigInt`. */
  @Benchmark def serializeBigInt: Array[Byte] = Json.toBytes(eagerBigIntNumber)

  private val eagerBigDecimalNumber: JsNumber = JsNumber(bigDecimalValue)

  /** Serializes a `JsNumber` from a `BigDecimal`. */
  @Benchmark def serializeBigDecimal: Array[Byte] = Json.toBytes(eagerBigDecimalNumber)

  private val eagerLargeBigDecimalNumber: JsNumber = JsNumber(largeBigDecimalValue)

  /** Serializes a `JsNumber` from a large `BigDecimal`. */  
  @Benchmark def serializeLargeBigDecimal: Array[Byte] = Json.toBytes(eagerLargeBigDecimalNumber)
}
