/**************************************************************************************************
Rapture Web Library
Version 0.8.0

The primary distribution site is

  http://www.propensive.com/

Copyright 2010-2012 Propensive Ltd.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied. See the License for the specific language governing permissions and limitations under the
License.
***************************************************************************************************/
package rapture.web
import java.nio._
import java.io._

import rapture.io._
import rapture.fs._
import rapture.time._

/** A collection of standard response-related objects. */
object Response {
  val NotModified = ErrorResponse(304, Nil, "Not Modified", "Resource is unchanged.")
  val Forbidden = ErrorResponse(403, Nil, "Forbidden", "Access to the requested resource is denied.")
  val NotFound = ErrorResponse(404, Nil, "Not Found", "The requested resource could not be located.")

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
}

case class BufferResponse(code: Int, headers: Seq[(String, String)],
    contentType: MimeTypes.MimeType, buffers: Array[ByteBuffer]) extends Response
case class StreamResponse(code: Int, headers: Seq[(String, String)],
    contentType: MimeTypes.MimeType, send: Output[Char] => Unit)(implicit enc: Encodings.Encoding) extends Response {
  def encoding = enc
}
case class ByteStreamResponse(code: Int, headers: Seq[(String, String)],
    contentType: MimeTypes.MimeType, send: Output[Byte] => Unit) extends Response
case class ErrorResponse(code: Int, headers: Seq[(String, String)],
    message: String, detail: String) extends Response
case class FileResponse(code: Int, headers: Seq[(String, String)],
    contentType: MimeTypes.MimeType, file: FileUrl) extends Response
case class RedirectResponse(headers: Seq[(String, String)], location: String) extends Response {
  final def code = 302
}

case class Cached[T](toCache: T, lastModified: DateTime)
case class Attachment[T](original: T, filename: String)
