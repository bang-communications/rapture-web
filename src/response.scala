package rapture.web
import java.nio._
import java.io._

import rapture.io._

/** A collection of standard response-related objects. */
object Response {
  val NotModified = ErrorResponse(304, Nil, Nil, "Not Modified", "Resource is unchanged.")
  val Forbidden = ErrorResponse(403, Nil, Nil, "Forbidden", "Access to the requested resource is denied.")
  val NotFound = ErrorResponse(404, Nil, Nil, "Not Found", "The requested resource could not be located.")

  val NoCache = List(
    "Expires" -> "Sat, 6 May 1995 12:00:00 GMT",
    "Cache-Control" -> "no-store, no-cache, must-revalidate, post-check=0, pre-check=0",
    "Pragma" -> "no-cache"
  )
}

/** Basic definition for a response */
sealed trait Response {
  def code: Int
  def headers: Seq[(String, String)]
  def cookies: Seq[ResponseCookie]
}

case class BufferResponse(code: Int, headers: Seq[(String, String)],
    cookies: Seq[ResponseCookie], contentType: MimeTypes.MimeType,
    buffers: Array[ByteBuffer]) extends Response
case class StreamResponse(code: Int, headers: Seq[(String, String)],
    cookies: Seq[ResponseCookie], contentType: MimeTypes.MimeType,
    send: Output[Char] => Unit)(implicit enc: Encodings.Encoding) extends Response {
  def encoding = enc
}
case class ErrorResponse(code: Int, headers: Seq[(String, String)],
    cookies: Seq[ResponseCookie], message: String, detail: String) extends Response
case class FileResponse(code: Int, headers: Seq[(String, String)],
    cookies: Seq[ResponseCookie], contentType: MimeTypes.MimeType,
    file: FileUrl) extends Response
case class RedirectResponse(headers: Seq[(String, String)],
    cookies: Seq[ResponseCookie], location: String) extends Response {
  final def code = 302
}

case class ResponseCookie(domain: String, expires: Option[Long], name: String,
    path: String, secure: Boolean, value: String)
