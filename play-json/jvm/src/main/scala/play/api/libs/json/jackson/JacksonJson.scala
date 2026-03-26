/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json.jackson

import java.io.InputStream
import java.io.OutputStream
import java.io.StringWriter

import scala.annotation.switch
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

import com.fasterxml.jackson.annotation.JsonFormat
import tools.jackson.core.{ JsonGenerator, JsonParser, JsonTokenId, Version }
import tools.jackson.core.json.{ JsonFactory, JsonWriteFeature }
import tools.jackson.databind.JacksonModule.SetupContext
import tools.jackson.databind._
import tools.jackson.databind.`type`.TypeFactory
import tools.jackson.databind.cfg.DateTimeFeature
import tools.jackson.databind.deser.Deserializers
import tools.jackson.databind.json.JsonMapper
import tools.jackson.databind.module.SimpleModule
import tools.jackson.databind.ser.Serializers
import tools.jackson.databind.util.TokenBuffer
import tools.jackson.module.scala.DefaultScalaModule

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

private[jackson] class JsValueSerializer(jsonConfig: JsonConfig) extends ValueSerializer[JsValue] {
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

  override def serialize(value: JsValue, json: JsonGenerator, ctxt: SerializationContext): Unit = {
    value match {
      case JsNumber(v) => {
        // Workaround #3784: Same behaviour as if JsonGenerator were
        // configured with WRITE_BIGDECIMAL_AS_PLAIN, but forced as this
        // configuration is ignored when called from ObjectMapper.valueToTree
        val shouldWritePlain = {
          val va = v.abs
          va <= jsonConfig.bigDecimalSerializerConfig.maxPlain && va >= jsonConfig.bigDecimalSerializerConfig.minPlain
        }
        val stripped               = stripTrailingZeros(v.bigDecimal)
        val (raw, rawAsBigDecimal) = if (shouldWritePlain) {
          val str = stripped.toPlainString
          (str, new JBigDec(str))
        } else {
          (stripped.toString, stripped)
        }

        json match {
          case tb: TokenBuffer =>
            // If the JsonGenerator is a TokenBuffer, use its writeNumber specific method
            val isInteger = raw.forall(c => c != 'E' && c != '.')
            tb.writeNumber(raw, isInteger)
          case _ =>
            json.writeNumber(rawAsBigDecimal)
        }
      }

      case JsString(v)  => json.writeString(v)
      case JsBoolean(v) => json.writeBoolean(v)

      case JsArray(elements) => {
        json.writeStartArray()
        elements.foreach { t =>
          serialize(t, json, ctxt)
        }
        json.writeEndArray()
      }

      case JsObject(values) => {
        json.writeStartObject()
        values.foreach { t =>
          json.writeName(t._1)
          serialize(t._2, json, ctxt)
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

private[jackson] case class ReadingList(content: mutable.ArrayBuffer[JsValue]) extends DeserializerContext {
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
    extends ValueDeserializer[Object] {
  override def isCachable: Boolean = true

  override def deserialize(jp: JsonParser, ctxt: DeserializationContext): JsValue = {
    val value = deserialize(jp, ctxt, List())

    if (!klass.isAssignableFrom(value.getClass)) {
      ctxt.handleUnexpectedToken(klass, jp)
    }
    value
  }

  private def parseBigDecimal(
      jp: JsonParser,
      parserContext: List[DeserializerContext]
  ): (Some[JsNumber], List[DeserializerContext]) = {
    BigDecimalParser.parse(jp.getString, jsonConfig) match {
      case JsSuccess(bigDecimal, _) =>
        (Some(JsNumber(bigDecimal)), parserContext)

      case JsError((_, JsonValidationError("error.expected.numberdigitlimit" +: _) +: _) +: _) =>
        throw new IllegalArgumentException(s"Number is larger than supported for field '${jp.currentName}'")

      case JsError((_, JsonValidationError("error.expected.numberscalelimit" +: _, args @ _*) +: _) +: _) =>
        val scale = args.headOption.fold("")(scale => s" ($scale)")
        throw new IllegalArgumentException(s"Number scale$scale is out of limits for field '${jp.currentName}'")

      case JsError((_, JsonValidationError("error.expected.numberformatexception" +: _) +: _) +: _) =>
        throw new NumberFormatException

      case JsError(errors) =>
        throw JsResultException(errors)
    }
  }

  @tailrec
  final def deserialize(
      jp: JsonParser,
      ctxt: DeserializationContext,
      parserContext: List[DeserializerContext]
  ): JsValue = {
    if (jp.currentToken() == null) {
      jp.nextToken() // happens when using treeToValue (we're not parsing tokens)
    }

    val valueAndCtx = (jp.currentToken().id(): @switch) match {
      case JsonTokenId.ID_NUMBER_INT | JsonTokenId.ID_NUMBER_FLOAT => parseBigDecimal(jp, parserContext)

      case JsonTokenId.ID_STRING => (Some(JsString(jp.getString)), parserContext)

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

      case JsonTokenId.ID_PROPERTY_NAME =>
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
  override def getNullValue(ctxt: DeserializationContext) = JsNull
}

private[jackson] class PlayDeserializers(jsonSettings: JsonConfig) extends Deserializers.Base {
  override def findBeanDeserializer(
      javaType: JavaType,
      config: DeserializationConfig,
      beanDescRef: BeanDescription.Supplier
  ) = {
    val klass = javaType.getRawClass
    if (classOf[JsValue].isAssignableFrom(klass) || klass == JsNull.getClass) {
      new JsValueDeserializer(config.getTypeFactory, klass, jsonSettings)
    } else null
  }

  override def hasDeserializerFor(config: DeserializationConfig, valueType: Class[?]): Boolean = {
    classOf[JsValue].isAssignableFrom(valueType) || valueType == JsNull.getClass
  }
}

private[jackson] class PlaySerializers(jsonSettings: JsonConfig) extends Serializers.Base {
  override def findSerializer(
      config: SerializationConfig,
      javaType: JavaType,
      beanDescRef: BeanDescription.Supplier,
      formatOverrides: JsonFormat.Value
  ) = {
    val ser: Object = if (classOf[JsValue].isAssignableFrom(beanDescRef.getBeanClass)) {
      new JsValueSerializer(jsonSettings)
    } else {
      null
    }
    ser.asInstanceOf[ValueSerializer[Object]]
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
  private var currentMapper: JsonMapper = null
  private val defaultMapper: JsonMapper = JsonMapper
    .builder(
      JsonFactory
        .builder()
        .streamReadConstraints(defaultMapperJsonConfig.streamReadConstraints)
        .streamWriteConstraints(defaultMapperJsonConfig.streamWriteConstraints)
        .build()
    )
    .addModules(
      new DefaultScalaModule(),
      new PlayJsonMapperModule(defaultMapperJsonConfig),
    )
    .disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES)
    .disable(DateTimeFeature.WRITE_DATES_AS_TIMESTAMPS)
    .disable(DateTimeFeature.WRITE_DURATIONS_AS_TIMESTAMPS)
    .disable(SerializationFeature.FAIL_ON_EMPTY_BEANS)
    .build()

  private[play] def mapper(): JsonMapper = if (currentMapper == null) {
    defaultMapper
  } else {
    currentMapper
  }

  private[play] def setObjectMapper(mapper: JsonMapper): Unit = {
    this.currentMapper = mapper
  }

  def parseJsValue(data: Array[Byte]): JsValue =
    mapper().readValue(mapper().createParser(data), classOf[JsValue])

  def parseJsValue(input: String): JsValue =
    mapper().readValue(mapper().createParser(input), classOf[JsValue])

  def parseJsValue(stream: InputStream): JsValue =
    mapper().readValue(mapper().createParser(stream), classOf[JsValue])

  private def withStringWriter[T](f: StringWriter => T): T = {
    val sw = new StringWriter()

    try {
      f(sw)
    } catch {
      case err: Throwable => throw err
    } finally {
      if (sw != null) try {
        sw.close()
      } catch {
        case _: Throwable => ()
      }
    }
  }

  def generateFromJsValue(jsValue: JsValue, escapeNonASCII: Boolean): String =
    withStringWriter { sw =>
      val mapperWithEscapeNonASCII = if (escapeNonASCII) {
        mapper().rebuild().enable(JsonWriteFeature.ESCAPE_NON_ASCII).build()
      } else {
        mapper()
      }

      mapperWithEscapeNonASCII.writeValue(sw, jsValue)
      sw.flush()
      sw.getBuffer.toString
    }

  def prettyPrint(jsValue: JsValue): String = withStringWriter { sw =>
    val writer: ObjectWriter = mapper().writerWithDefaultPrettyPrinter()

    writer.writeValue(sw, jsValue)
    sw.flush()
    sw.getBuffer.toString
  }

  def prettyPrintToStream(jsValue: JsValue, stream: OutputStream): Unit = {
    val writer: ObjectWriter = mapper().writerWithDefaultPrettyPrinter()

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
