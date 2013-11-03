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

import scala.xml._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ListMap

import scala.concurrent._

import javax.servlet.http._

import rapture.io._
import rapture.core._
import rapture.time._
import rapture.json._
import rapture.fs._

import strategy.throwExceptions

trait LpRequestHandlers { this: HttpServer =>
  implicit val linkRedirectHandler = new Handler[Link] {
    def response(link: Link) = RedirectResponse(Nil, link.toString)
  }
}

trait RequestHandlers extends LpRequestHandlers { this: HttpServer =>

  trait Handler[-T] { def response(t: T): Response }
  
  implicit def charInputHandler(implicit enc: Encodings.Encoding) = new Handler[Input[Char]] {
    def response(in: Input[Char]) = StreamResponse(200, Response.NoCache,
        MimeTypes.`text/plain`, { os =>
      in > os
      os.close()
    })
  }

  implicit val StringInputHandler =
    new Handler[Input[String]] {
      implicit val enc = Encodings.`UTF-8`
      def response(in: Input[String]) = StreamResponse(200, Response.NoCache,
          MimeTypes.`text/plain`, { os =>
        var ln = in.read()
        while(ln != None) {
          (ln+"\n").input > os
          ln = in.read()
        }
        os.close()
      })
    }

  implicit def xmlHandler(implicit enc: Encodings.Encoding) = new Handler[Seq[Node]] {
    def response(t: Seq[Node]) = StreamResponse(200, Response.NoCache,
        MimeTypes.`application/xml`, { os =>
      ("<?xml version=\"1.0\" encoding=\""+enc.name+"\"?>\n").input > os
      t.toString.input > os
      os.close()
    })
  }
  
  implicit def csvHandler(implicit enc: Encodings.Encoding) = new Handler[Csv] {
    def response(csv: Csv) = StreamResponse(200, List("Pragma" -> "public",
        "Content-Disposition" -> "attachment;filename=data.csv",
        "Expires" -> "Sat, 6 May 1995 12:00:00 GMT",
        "Cache-Control" -> "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"
        ),
        MimeTypes.`text/csv`, { os =>
      csv.toString.input > os
      os.close()
    })
  }
  
  implicit def cssHandler(implicit enc: Encodings.Encoding) = new Handler[HtmlCss.Stylesheet] {
    def response(css: HtmlCss.Stylesheet) = StreamResponse(200, Response.NoCache,
        MimeTypes.`text/css`, { os =>
      css.toString.input > os
      os.close()
    })
  }
  
  implicit def stringHandler(implicit enc: Encodings.Encoding) = new Handler[String] {
    def response(t: String) = StreamResponse(200, Response.NoCache,
        MimeTypes.`text/plain`, { os =>
      t.input > os
      os.close()
    })
  }
  
  implicit val byteInputHandler = new Handler[Input[Byte] with TypedInput] {
    def response(in: (Input[Byte] with TypedInput)) = ByteStreamResponse(200, Response.NoCache,
        in.mimeType, { os =>
      in > os
      os.close()
    })
  }

  implicit def fileHandler = new Handler[FileUrl] {
    def response(file: FileUrl) = FileResponse(200, Response.NoCache,
        file.extension.toList.flatMap(MimeTypes.extension).headOption.getOrElse(
	      MimeTypes.`text/plain`), file)
  }

  implicit def cacheHandler[T](implicit h: Handler[T]): Handler[Cached[T]] = new Handler[Cached[T]] {
    def response(resp: Cached[T]) = {
      val r = h.response(resp.toCache)
      val dateFormat = DateFormat("EEE, d MMM yyyy")
      val timeFormat = TimeFormat("HH:mm:ss z")
      val lastModified = List("Last-modified" -> resp.lastModified.format(dateFormat, timeFormat))
      r match {
        case BufferResponse(code, headers, contentType, buffers) =>
          BufferResponse(code, lastModified, contentType, buffers)
        case sr@StreamResponse(code, headers, contentType, send) =>
          StreamResponse(code, lastModified, contentType, send)(sr.encoding)
        case ByteStreamResponse(code, headers, contentType, send) =>
          ByteStreamResponse(code, lastModified, contentType, send)
        case ErrorResponse(code, headers, message, detail) =>
          ErrorResponse(code, lastModified, message, detail)
        case FileResponse(code, headers, contentType, file) =>
          FileResponse(code, lastModified, contentType, file)
        case RedirectResponse(headers, location) =>
          RedirectResponse(lastModified, location)
      }
    }
  }

  implicit def attachmentHandler[T](implicit h: Handler[T]): Handler[Attachment[T]] = new Handler[Attachment[T]] {
    def response(resp: Attachment[T]) = {
      val r = h.response(resp.original)
      val headers = ("Content-Disposition" -> ("attachment; filename="+resp.filename)) :: r.headers.toList
      r match {
        case BufferResponse(code, headers, contentType, buffers) =>
          BufferResponse(code, headers, contentType, buffers)
        case sr@StreamResponse(code, headers, contentType, send) =>
          StreamResponse(code, headers, contentType, send)(sr.encoding)
        case ByteStreamResponse(code, headers, contentType, send) =>
          ByteStreamResponse(code, headers, contentType, send)
        case ErrorResponse(code, headers, message, detail) =>
          ErrorResponse(code, headers, message, detail)
        case FileResponse(code, headers, contentType, file) =>
          FileResponse(code, headers, contentType, file)
        case RedirectResponse(headers, location) =>
          RedirectResponse(headers, location)
      }
    }
  }

  implicit def futureHandler[T](implicit h: Handler[T], ec: ExecutionContext): Handler[Future[T]] =
    new Handler[Future[T]] {
      def response(future: Future[T]) = h.response(Await.result(future, duration.Duration.Inf))
    }

  implicit val nullHandler = new Handler[Response] { def response(r: Response) = r }

  implicit def jsonHandler(implicit enc: Encodings.Encoding) = new Handler[Json] {
    def response(t: Json) = StreamResponse(200, Response.NoCache,
        MimeTypes.`application/json`, { os =>
      t.toString.input > os
      os.close()
    })
  }

  implicit def pageHandler(implicit enc: Encodings.Encoding) = new Handler[Layout.Page] {
    def response(page: Layout.Page) = StreamResponse(page.httpStatus, Response.NoCache,
        MimeTypes.`text/html`, { os =>
      page.stream > os
    })
  }
}
