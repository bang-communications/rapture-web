package rapture.web

import scala.xml._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ListMap

import javax.servlet.http._

import rapture.io._

import rapture.html._


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
  
  implicit def stringHandler(implicit enc: Encodings.Encoding) = new Handler[String] {
    def response(t: String) = StreamResponse(200, Response.NoCache,
        MimeTypes.`text/plain`, { os =>
      t.input > os
      os.close()
    })
  }
  
  implicit def fileHandler = new Handler[FileUrl] {
    def response(file: FileUrl) = FileResponse(200, Response.NoCache,
        file.extension.toList.flatMap(MimeTypes.extension).headOption.getOrElse(
	      MimeTypes.`text/plain`), file)
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
