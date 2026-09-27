/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json.jackson

import java.io.InputStream
import java.io.OutputStream

import scala.annotation.{ switch, tailrec }

import scala.collection.mutable.{ ArrayBuffer, ListBuffer }

import com.fasterxml.jackson.core.JsonFactoryBuilder
import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.core.JsonTokenId
import com.fasterxml.jackson.core.JsonToken
import com.fasterxml.jackson.core.StreamWriteFeature
import com.fasterxml.jackson.core.Version
import com.fasterxml.jackson.core.json.JsonWriteFeature

import com.fasterxml.jackson.databind.Module.SetupContext
import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.databind.`type`.TypeFactory
import com.fasterxml.jackson.databind.deser.Deserializers
import com.fasterxml.jackson.databind.json.JsonMapper
import com.fasterxml.jackson.databind.module.SimpleModule
import com.fasterxml.jackson.databind.ser.Serializers
import com.fasterxml.jackson.databind.util.TokenBuffer
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule
import com.fasterxml.jackson.module.paramnames.ParameterNamesModule
import com.fasterxml.jackson.module.scala.DefaultScalaModule

import play.api.libs.json._

/**
 * The Play JSON module for Jackson.
 *
 * This can be used if you want to use a custom Jackson ObjectMapper, or more advanced Jackson features when working
 * with JsValue.  To use this:
 *
 * {{{
 * import com.fasterxml.jackson.databind.ObjectMapper
 *
 * import play.api.libs.json.JsValue
 * import play.api.libs.json.jackson.PlayJsonMapperModule
 * import play.api.libs.json.JsonConfig
 *
 * val jsonSettings = JsonConfig.settings
 * val mapper = new ObjectMapper().registerModule(
 *   new PlayJsonMapperModule(jsonSettings))
 * val jsValue = mapper.readValue("""{"foo":"bar"}""", classOf[JsValue])
 * }}}
 */
sealed class PlayJsonMapperModule(jsonConfig: JsonConfig) extends SimpleModule("PlayJson", Version.unknownVersion()) {
  def this() = this(JsonConfig.settings)
  override def setupModule(context: SetupContext): Unit = {
    context.addDeserializers(new PlayDeserializers(jsonConfig))
    context.addSerializers(new PlaySerializers(jsonConfig))
  }
}

// -- Serializers.

private[jackson] class JsValueSerializer(jsonConfig: JsonConfig) extends JsonSerializer[JsValue] {
  import java.math.{ BigDecimal => JBigDec }

  private def stripTrailingZeros(bigDec: JBigDec): JBigDec = {
    val stripped = bigDec.stripTrailingZeros

    if (jsonConfig.bigDecimalSerializerConfig.preserveZeroDecimal && bigDec.scale > 0 && stripped.scale <= 0) {
      // restore .0 if rounded to a whole number
      stripped.setScale(1)
    } else {
      stripped
    }
  }

  @scala.annotation.nowarn("msg=.*outer\\ reference.*")
  override def serialize(value: JsValue, json: JsonGenerator, provider: SerializerProvider): Unit = {
    value match {
      case n: JsNumber.JsBigDecimal => {
        val v = n.value

        // Workaround #3784: Same behaviour as if JsonGenerator were
        // configured with WRITE_BIGDECIMAL_AS_PLAIN, but forced as this
        // configuration is ignored when called from ObjectMapper.valueToTree
        val shouldWritePlain = {
          val va = v.abs
          va <= jsonConfig.bigDecimalSerializerConfig.maxPlain && va >= jsonConfig.bigDecimalSerializerConfig.minPlain
        }
        val stripped = stripTrailingZeros(v.bigDecimal)
        val raw      = if (shouldWritePlain) stripped.toPlainString else stripped.toString

        if (raw.exists(c => c == 'E' || c == '.')) {
          json.writeNumber(raw)
        } else {
          json match {
            case tb: TokenBuffer =>
              tb.writeNumber(raw, true)

            case _ =>
              json.writeNumber(raw)
          }
        }
      }

      case n: JsNumber.JsBigInteger =>
        json.writeNumber(n.underlying.underlying)

      case n: JsNumber.JsLong =>
        json.writeNumber(n.underlying)

      case n: JsNumber.JsNumericInt[?] =>
        json.writeNumber(n.toInt)

      case n: JsNumber =>
        json.writeNumber(n.text)

      case JsString(v)  => json.writeString(v)
      case JsBoolean(v) => json.writeBoolean(v)

      case JsArray(elements) => {
        json.writeStartArray()
        elements.foreach { t =>
          serialize(t, json, provider)
        }
        json.writeEndArray()
      }

      case JsObject(values) => {
        json.writeStartObject()
        values.foreach { t =>
          json.writeFieldName(t._1)
          serialize(t._2, json, provider)
        }
        json.writeEndObject()
      }

      case JsNull => json.writeNull()
    }
  }
}

private[jackson] sealed trait DeserializerContext {
  def addValue(value: JsValue): DeserializerContext
}

private[jackson] case class ReadingList(content: ArrayBuffer[JsValue]) extends DeserializerContext {
  override def addValue(value: JsValue): DeserializerContext = {
    ReadingList(content += value)
  }
}

// Context for reading an Object
private[jackson] case class KeyRead(content: ListBuffer[(String, JsValue)], fieldName: String)
    extends DeserializerContext {
  def addValue(value: JsValue): DeserializerContext = ReadingMap(content += (fieldName -> value))
}

// Context for reading one item of an Object (we already red fieldName)
private[jackson] case class ReadingMap(content: ListBuffer[(String, JsValue)]) extends DeserializerContext {
  def setField(fieldName: String)                   = KeyRead(content, fieldName)
  def addValue(value: JsValue): DeserializerContext =
    throw new Exception("Cannot add a value on an object without a key, malformed JSON object!")
}

private[jackson] class JsValueDeserializer(factory: TypeFactory, klass: Class[?], jsonConfig: JsonConfig)
    extends JsonDeserializer[Object] {
  override def isCachable: Boolean = true

  override def deserialize(jp: JsonParser, ctxt: DeserializationContext): JsValue = {
    val value = deserialize(jp, ctxt, List.empty)

    if (!klass.isAssignableFrom(value.getClass)) {
      ctxt.handleUnexpectedToken(klass, jp)
    }

    value
  }

  private def parseNumber(
      jp: JsonParser,
      parserContext: List[DeserializerContext]
  ): (JsNumber, List[DeserializerContext]) = {
    val buf: Array[Char] = jp.getTextCharacters
    val off: Int         = jp.getTextOffset
    val len: Int         = jp.getTextLength
    val tok              = jp.currentToken

    import com.fasterxml.jackson.core.io.NumberInput

    val parsed = tok match {
      case JsonToken.VALUE_NUMBER_INT => {
        val negative = len > 0 && buf(off) == '-'

        // Strip the sign for Jackson's digit-only char[] utilities
        val numOff = if (negative) off + 1 else off
        val numLen = if (negative) len - 1 else len

        if (numLen <= 9) {
          // 1. Safe for 1-9 digits: Jackson's parseInt requires unsigned digit length
          val absValue   = NumberInput.parseInt(buf, numOff, numLen)
          val finalValue = if (negative) -absValue else absValue

          JsNumber(finalValue)
        } else if (numLen <= 18) {
          // 2. Safe for 10-18 digits: Jackson's parseLong usage
          val absValue   = NumberInput.parseLong(buf, numOff, numLen)
          val finalValue = if (negative) -absValue else absValue

          JsNumber(finalValue)
        } else if (NumberInput.inLongRange(buf, numOff, numLen, negative)) {
          // 3. Safe for exactly 19 digits (e.g. Long.MaxValue boundaries)
          // Use string-based parsing to let Jackson handle the safe 64-bit bounds step
          JsNumber(NumberInput.parseLong(new String(buf, off, len)))
        } else {
          // 4. Fallback: Handles giant integers outside Long limits safely
          // Allocates a temporary String slice to shield
          // the JVM from raw indexing math crashes.
          JsNumber(BigInt(new String(buf, off, len)))
        }
      }

      case JsonToken.VALUE_NUMBER_FLOAT => {
        if (len > jsonConfig.bigDecimalParseConfig.digitsLimit) {
          throw new IllegalArgumentException(s"Number is larger than supported for field '${jp.currentName}'")
        }

        // Zero-allocation exponent detection
        def hasExponent: Boolean = {
          var found = false
          var i     = off
          val end   = off + len

          while (i < end && !found) {
            val c = buf(i)
            if (c == 'e' || c == 'E') found = true
            i += 1
          }

          found
        }

        if (len <= 15 && !hasExponent) {
          // Parse directly from Jackson's char buffer, avoiding an intermediate String.

          val double = NumberInput.parseDouble(buf, off, len, true)
          val text   = new String(buf, off, len)

          new JsNumber.JsNumericDouble(
            underlying = double,
            repr = BigDecimal(new java.math.BigDecimal(text, jsonConfig.bigDecimalParseConfig.mathContext)),
            float = double.toFloat
          ) // TODO: Keep text
        } else {
          // Instantiate BigDecimal cleanly using structural slice parameters
          val bigDecimal = new java.math.BigDecimal(buf, off, len, jsonConfig.bigDecimalParseConfig.mathContext)

          if (math.abs(bigDecimal.scale) > jsonConfig.bigDecimalParseConfig.scaleLimit) {
            throw new IllegalArgumentException(
              s"Number scale is out of limits for field '${jp.currentName}': ${bigDecimal.scale} > ${jsonConfig.bigDecimalParseConfig.scaleLimit}"
            )
          } else {
            new JsNumber.JsLazy(
              numberType = JsNumber.NumberType.Float,
              text = new String(buf, off, len),
              bigDecimal = bigDecimal
            )
          }
        }
      }

      case _ =>
        throw new NumberFormatException("Expected a numeric JSON token")
    }

    parsed -> parserContext
  }

  @tailrec
  final def deserialize(
      jp: JsonParser,
      ctxt: DeserializationContext,
      parserContext: List[DeserializerContext]
  ): JsValue = {
    if (jp.getCurrentToken == null) {
      jp.nextToken() // happens when using treeToValue (we're not parsing tokens)
    }

    val valueAndCtx = (jp.getCurrentToken.id(): @switch) match {
      case JsonTokenId.ID_NUMBER_INT | JsonTokenId.ID_NUMBER_FLOAT => {
        val (num, ctx) = parseNumber(jp, parserContext)
        Some(num) -> ctx
      }

      case JsonTokenId.ID_STRING => (Some(JsString(jp.getText)), parserContext)

      case JsonTokenId.ID_TRUE => (Some(JsBoolean(true)), parserContext)

      case JsonTokenId.ID_FALSE => (Some(JsBoolean(false)), parserContext)

      case JsonTokenId.ID_NULL => (Some(JsNull), parserContext)

      case JsonTokenId.ID_START_ARRAY => (None, ReadingList(ArrayBuffer()) +: parserContext)

      case JsonTokenId.ID_END_ARRAY =>
        parserContext match {
          case ReadingList(content) :: stack => (Some(JsArray(content)), stack)
          case _ => throw new RuntimeException("We should have been reading list, something got wrong")
        }

      case JsonTokenId.ID_START_OBJECT => (None, ReadingMap(ListBuffer()) +: parserContext)

      case JsonTokenId.ID_FIELD_NAME =>
        parserContext match {
          case (c: ReadingMap) :: stack => (None, c.setField(jp.currentName()) +: stack)
          case _                        => throw new RuntimeException("We should be reading map, something got wrong")
        }

      case JsonTokenId.ID_END_OBJECT =>
        parserContext match {
          case ReadingMap(content) :: stack => (Some(JsObject(content)), stack)
          case _ => throw new RuntimeException("We should have been reading an object, something got wrong")
        }

      case JsonTokenId.ID_NOT_AVAILABLE =>
        throw new RuntimeException("We should have been reading an object, something got wrong")

      case JsonTokenId.ID_EMBEDDED_OBJECT =>
        throw new RuntimeException("We should have been reading an object, something got wrong")
    }

    // Read ahead
    jp.nextToken()

    valueAndCtx match {
      case (Some(v), Nil)               => v // done, no more tokens and got a value!
      case (Some(v), previous :: stack) => deserialize(jp, ctxt, previous.addValue(v) :: stack)
      case (None, nextContext)          => deserialize(jp, ctxt, nextContext)
    }
  }

  // This is used when the root object is null, ie when deserializing "null"
  override val getNullValue = JsNull
}

private[jackson] class PlayDeserializers(jsonSettings: JsonConfig) extends Deserializers.Base {
  override def findBeanDeserializer(javaType: JavaType, config: DeserializationConfig, beanDesc: BeanDescription) = {
    val klass = javaType.getRawClass

    if (classOf[JsValue].isAssignableFrom(klass) || klass == JsNull.getClass) {
      new JsValueDeserializer(config.getTypeFactory, klass, jsonSettings)
    } else null
  }
}

private[jackson] class PlaySerializers(jsonSettings: JsonConfig) extends Serializers.Base {
  override def findSerializer(config: SerializationConfig, javaType: JavaType, beanDesc: BeanDescription) = {
    val ser: Object = if (classOf[JsValue].isAssignableFrom(beanDesc.getBeanClass)) {
      new JsValueSerializer(jsonSettings)
    } else {
      null
    }

    ser.asInstanceOf[JsonSerializer[Object]]
  }
}

private[play] object JacksonJson {
  private var instance = JacksonJson(JsonConfig.settings)

  /** Overrides the config. */
  private[play] def setConfig(jsonConfig: JsonConfig): Unit = {
    instance = JacksonJson(jsonConfig)
  }

  private[play] def get: JacksonJson = instance
}

private[play] case class JacksonJson(defaultMapperJsonConfig: JsonConfig) {
  private var currentMapper: ObjectMapper = null

  private val defaultMapper: ObjectMapper = JsonMapper
    .builder(
      new JsonFactoryBuilder()
        .streamReadConstraints(defaultMapperJsonConfig.streamReadConstraints)
        .streamWriteConstraints(defaultMapperJsonConfig.streamWriteConstraints)
        .build()
    )
    .addModules(
      new ParameterNamesModule(),
      new Jdk8Module(),
      new JavaTimeModule(),
      new DefaultScalaModule(),
      new PlayJsonMapperModule(defaultMapperJsonConfig),
    )
    .disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES)
    .disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS)
    .disable(SerializationFeature.WRITE_DURATIONS_AS_TIMESTAMPS)
    .disable(SerializationFeature.FAIL_ON_EMPTY_BEANS)
    .build()

  private[play] def mapper(): ObjectMapper = if (currentMapper == null) {
    defaultMapper
  } else {
    currentMapper
  }

  private[play] def setObjectMapper(mapper: ObjectMapper): Unit = {
    this.currentMapper = mapper
  }

  def parseJsValue(data: Array[Byte]): JsValue =
    mapper().readValue(data, classOf[JsValue])

  def parseJsValue(input: String): JsValue =
    mapper().readValue(input, classOf[JsValue])

  def parseJsValue(stream: InputStream): JsValue =
    mapper().readValue(stream, classOf[JsValue])

  def generateFromJsValue(jsValue: JsValue, escapeNonASCII: Boolean): String = {
    val writer           = mapper().writer()
    val configuredWriter = if (escapeNonASCII) {
      writer.`with`(JsonWriteFeature.ESCAPE_NON_ASCII)
    } else {
      writer
    }

    configuredWriter.writeValueAsString(jsValue)
  }

  def prettyPrint(jsValue: JsValue): String = {
    val writer: ObjectWriter = mapper().writerWithDefaultPrettyPrinter()
    writer.writeValueAsString(jsValue)
  }

  def prettyPrintToStream(jsValue: JsValue, stream: OutputStream): Unit = {
    val writer: ObjectWriter = mapper()
      .writerWithDefaultPrettyPrinter()
      .without(StreamWriteFeature.AUTO_CLOSE_TARGET)
    writer.writeValue(stream, jsValue)
  }

  def jsValueToBytes(jsValue: JsValue): Array[Byte] =
    mapper().writeValueAsBytes(jsValue)

  def writeJsValueToStream(jsValue: JsValue, stream: OutputStream): Unit =
    mapper().writeValue(stream, jsValue)

  def jsValueToJsonNode(jsValue: JsValue): JsonNode =
    mapper().valueToTree(jsValue)

  def jsonNodeToJsValue(jsonNode: JsonNode): JsValue =
    mapper().treeToValue(jsonNode, classOf[JsValue])
}
