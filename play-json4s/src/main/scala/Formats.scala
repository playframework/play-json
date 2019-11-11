package play.api.libs.json.json4s

import scala.util.control.NonFatal

import org.json4s.{ JValue, Reader, Writer }

import play.api.libs.json.{
  JsError,
  JsResult,
  JsSuccess,
  JsValue,
  Reads,
  Writes
}

object Formats extends Formats

sealed trait Formats extends LowPriorityFormats {
  /**
   * {{{
   * def test[T](implicit w: Reads[T]): Reader[T] = Formats.reader[T]
   * }}}
   */
  implicit def reader[T](implicit reads: Reads[T], conv: JValue => JsValue = JValueConverters.jvalue2JsValue): Reader[T] = new JsValueReader[T](reads, conv)

  /**
   * {{{
   * def test[T](implicit w: Writes[T]): Writer[T] = Formats.defaultWriter[T]
   * }}}
   */
  implicit def defaultWriter[T](implicit writes: Writes[T]): Writer[T] =
    new JsValueWriter[T](writes, JValueConverters.jsvalue2JValue)

  /**
   * {{{
   * def test[T](implicit r: Reader[T]): Reads[T] = Formats.reads[T]
   * }}}
   */
  implicit def reads[T](implicit reader: Reader[T], conv: JsValue => JValue): Reads[T] = reads[T]((_: JsValue, cause: Throwable) => JsError(cause.getMessage))

  /**
   * {{{
   * def test[T](implicit r: Reader[T]): Reads[T] =
   *   Formats.reads[T](err = { (v: JsValue, cause: Throwable) =>
   *     JsError(s"Custom message: \$v; \$cause")
   *   })
   * }}}
   */
  def reads[T](err: (JsValue, Throwable) => JsError)(implicit reader: Reader[T], conv: JsValue => JValue): Reads[T] = new JValueReads[T](reader, conv, err)

  /**
   * {{{
   * def test[T](implicit w: Writer[T]): Writes[T] = Formats.defaultWrites[T]
   * }}}
   */
  implicit def defaultWrites[T](implicit writer: Writer[T]): Writes[T] =
    new JValueWrites[T](writer, JValueConverters.jvalue2JsValue)

}

private[json4s] sealed trait LowPriorityFormats { _: Formats =>
  /**
   * {{{
   * def test[T](implicit w: Reads[T]): Reader[T] = Formats.defaultReader[T]
   * }}}
   */
  implicit def defaultReader[T](implicit reads: Reads[T]): Reader[T] =
    new JsValueReader[T](reads, JValueConverters.jvalue2JsValue)

  /**
   * {{{
   * def test[T](implicit w: Writes[T]): Writer[T] = Formats.writer[T]
   * }}}
   */
  implicit def writer[T](implicit writes: Writes[T], conv: JsValue => JValue): Writer[T] = new JsValueWriter[T](writes, conv)

  /**
   * {{{
   * def test[T](implicit r: Reader[T]): Reads[T] = Formats.defaultReads[T]
   * }}}
   */
  implicit def defaultReads[T](implicit reader: Reader[T]): Reads[T] =
    reads[T]((_: JsValue, cause: Throwable) => JsError(cause.getMessage))(
      reader, JValueConverters.jsvalue2JValue)

  /**
   * {{{
   * def test[T](implicit w: Writer[T]): Writes[T] = Formats.writes[T]
   * }}}
   */
  implicit def writes[T](implicit writer: Writer[T], conv: JValue => JsValue): Writes[T] = new JValueWrites[T](writer, conv)
}

private[json4s] final class JsValueReader[T](
  r: Reads[T], conv: JValue => JsValue) extends Reader[T] {

  def read(value: JValue): T = r.reads(conv(value)).get
}

private[json4s] final class JsValueWriter[T](
  w: Writes[T], conv: JsValue => JValue) extends Writer[T] {

  def write(value: T): JValue = conv(w writes value)
}

private[json4s] final class JValueWrites[T](
  w: Writer[T],
  conv: JValue => JsValue) extends Writes[T] {
  def writes(value: T): JsValue = conv(w write value)
}

private[json4s] final class JValueReads[T](
  r: Reader[T],
  conv: JsValue => JValue,
  err: (JsValue, Throwable) => JsError) extends Reads[T] {
  def reads(js: JsValue): JsResult[T] = try {
    val v = r.read(conv(js))
    JsSuccess(v)
  } catch {
    case NonFatal(cause) => err(js, cause)
  }
}
