/*
 * Copyright (C) 2009-2020 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json.jackson

import java.io.InputStream
import java.io.StringWriter

import scala.annotation.switch
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

import com.fasterxml.jackson.core.JsonFactory
import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.core.JsonTokenId
import com.fasterxml.jackson.core.Version
import com.fasterxml.jackson.core.json.JsonWriteFeature
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter

import com.fasterxml.jackson.databind.Module.SetupContext
import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.databind.`type`.TypeFactory
import com.fasterxml.jackson.databind.deser.Deserializers
import com.fasterxml.jackson.databind.module.SimpleModule
import com.fasterxml.jackson.databind.ser.Serializers

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
 * import play.api.libs.json.jackson.PlayJsonModule
 * import play.api.libs.json.JsonParserSettings
 *
 * val jsonParseSettings = JsonParserSettings()
 * val mapper = new ObjectMapper().registerModule(
 *   new PlayJsonModule(jsonParseSettings))
 * val jsValue = mapper.readValue("""{"foo":"bar"}""", classOf[JsValue])
 * }}}
 */
sealed class PlayJsonModule(parserSettings: JsonParserSettings)
    extends SimpleModule("PlayJson", Version.unknownVersion()) {
  override def setupModule(context: SetupContext): Unit = {
    context.addDeserializers(new PlayDeserializers(parserSettings))
    context.addSerializers(new PlaySerializers(parserSettings))
  }
}

@deprecated("Use PlayJsonModule class instead", "2.6.11")
object PlayJsonModule extends PlayJsonModule(JsonParserSettings())

// -- Serializers.

private[jackson] class JsValueSerializer(parserSettings: JsonParserSettings) extends JsonSerializer[JsValue] {
  import java.math.BigInteger
  import java.math.{ BigDecimal => JBigDec }

  import com.fasterxml.jackson.databind.node.BigIntegerNode
  import com.fasterxml.jackson.databind.node.DecimalNode

  override def serialize(value: JsValue, json: JsonGenerator, provider: SerializerProvider): Unit = {
    value match {
      case JsNumber(v) => {
        // Workaround #3784: Same behaviour as if JsonGenerator were
        // configured with WRITE_BIGDECIMAL_AS_PLAIN, but forced as this
        // configuration is ignored when called from ObjectMapper.valueToTree
        val shouldWritePlain = {
          val va = v.abs
          va < parserSettings.bigDecimalSerializerSettings.maxPlain && va > parserSettings.bigDecimalSerializerSettings.minPlain
        }
        val stripped = v.bigDecimal.stripTrailingZeros
        val raw      = if (shouldWritePlain) stripped.toPlainString else stripped.toString

        if (raw.indexOf('E') < 0 && raw.indexOf('.') < 0)
          json.writeTree(new BigIntegerNode(new BigInteger(raw)))
        else
          json.writeTree(new DecimalNode(new JBigDec(raw)))
      }

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
  def setField(fieldName: String) = KeyRead(content, fieldName)
  def addValue(value: JsValue): DeserializerContext =
    throw new Exception("Cannot add a value on an object without a key, malformed JSON object!")
}

private[jackson] class JsValueDeserializer(factory: TypeFactory, klass: Class[_], parserSettings: JsonParserSettings)
    extends JsonDeserializer[Object] {
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
    BigDecimalParser.parse(jp.getText, parserSettings) match {
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
    if (jp.getCurrentToken == null) {
      jp.nextToken() // happens when using treeToValue (we're not parsing tokens)
    }

    val valueAndCtx = (jp.getCurrentToken.id(): @switch) match {
      case JsonTokenId.ID_NUMBER_INT | JsonTokenId.ID_NUMBER_FLOAT => parseBigDecimal(jp, parserContext)

      case JsonTokenId.ID_STRING => (Some(JsString(jp.getText)), parserContext)

      case JsonTokenId.ID_TRUE => (Some(JsBoolean(true)), parserContext)

      case JsonTokenId.ID_FALSE => (Some(JsBoolean(false)), parserContext)

      case JsonTokenId.ID_NULL => (Some(JsNull), parserContext)

      case JsonTokenId.ID_START_ARRAY => (None, ReadingList(ArrayBuffer()) +: parserContext)

      case JsonTokenId.ID_END_ARRAY =>
        parserContext match {
          case ReadingList(content) :: stack => (Some(JsArray(content)), stack)
          case _                             => throw new RuntimeException("We should have been reading list, something got wrong")
        }

      case JsonTokenId.ID_START_OBJECT => (None, ReadingMap(ListBuffer()) +: parserContext)

      case JsonTokenId.ID_FIELD_NAME =>
        parserContext match {
          case (c: ReadingMap) :: stack => (None, c.setField(jp.getCurrentName) +: stack)
          case _                        => throw new RuntimeException("We should be reading map, something got wrong")
        }

      case JsonTokenId.ID_END_OBJECT =>
        parserContext match {
          case ReadingMap(content) :: stack => (Some(JsObject(content)), stack)
          case _                            => throw new RuntimeException("We should have been reading an object, something got wrong")
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

private[jackson] class PlayDeserializers(parserSettings: JsonParserSettings) extends Deserializers.Base {
  override def findBeanDeserializer(javaType: JavaType, config: DeserializationConfig, beanDesc: BeanDescription) = {
    val klass = javaType.getRawClass
    if (classOf[JsValue].isAssignableFrom(klass) || klass == JsNull.getClass) {
      new JsValueDeserializer(config.getTypeFactory, klass, parserSettings)
    } else null
  }
}

private[jackson] class PlaySerializers(parserSettings: JsonParserSettings) extends Serializers.Base {
  override def findSerializer(config: SerializationConfig, javaType: JavaType, beanDesc: BeanDescription) = {
    val ser: Object = if (classOf[JsValue].isAssignableFrom(beanDesc.getBeanClass)) {
      new JsValueSerializer(parserSettings)
    } else {
      null
    }
    ser.asInstanceOf[JsonSerializer[Object]]
  }
}

private[json] object JacksonJson {
  private lazy val mapper = (new ObjectMapper).registerModule(new PlayJsonModule(JsonParserSettings.settings))

  private lazy val jsonFactory = new JsonFactory(mapper)

  private def stringJsonGenerator(out: java.io.StringWriter) =
    jsonFactory.createGenerator(out)

  def parseJsValue(data: Array[Byte]): JsValue =
    mapper.readValue(jsonFactory.createParser(data), classOf[JsValue])

  def parseJsValue(input: String): JsValue =
    mapper.readValue(jsonFactory.createParser(input), classOf[JsValue])

  def parseJsValue(stream: InputStream): JsValue =
    mapper.readValue(jsonFactory.createParser(stream), classOf[JsValue])

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
      val gen = stringJsonGenerator(sw)

      if (escapeNonASCII) {
        gen.enable(JsonWriteFeature.ESCAPE_NON_ASCII.mappedFeature)
      }

      mapper.writeValue(gen, jsValue)
      sw.flush()
      sw.getBuffer.toString
    }

  def prettyPrint(jsValue: JsValue): String = withStringWriter { sw =>
    val gen = stringJsonGenerator(sw).setPrettyPrinter(
      new DefaultPrettyPrinter()
    )
    val writer: ObjectWriter = mapper.writerWithDefaultPrettyPrinter()

    writer.writeValue(gen, jsValue)
    sw.flush()
    sw.getBuffer.toString
  }

  def jsValueToBytes(jsValue: JsValue): Array[Byte] =
    mapper.writeValueAsBytes(jsValue)

  def jsValueToJsonNode(jsValue: JsValue): JsonNode =
    mapper.valueToTree(jsValue)

  def jsonNodeToJsValue(jsonNode: JsonNode): JsValue =
    mapper.treeToValue(jsonNode, classOf[JsValue])
}
