package rapture.web
import javax.servlet.http._
import java.nio.ByteBuffer
import java.io._
import scala.collection.mutable._
import rapture.io._


/** Extend this to implement a Servlet. */
// FIXME: Needs a bit of work
abstract class ServletWrapper extends HttpServlet { wrapper =>
  implicit val zone = Zone("servlet")
 
  def baseUrl: String
  def secureBaseUrl: String
  def handle(req: Request): Response

  @inline def fromNull[T](t: T): Option[T] = if(t == null) None else Some(t)

  override def service(req: HttpServletRequest, resp: HttpServletResponse) = {
    val t0 = System.currentTimeMillis
    val vReq = try {
      new Request {
        def contentType = {
          val ct = req.getContentType
          val ct2 = if(ct == null) "application/x-www-form-urlencoded" else ct.split(";").head
          MimeTypes.fromString(ct2).getOrElse(MimeTypes.MimeType(ct2))
        }
        def contentLength = req.getContentLength
        def queryString = req.getQueryString
        def requestMethod = HttpMethods.method(req.getMethod)
        def scriptName = req.getRequestURI
        def https = req.isSecure
        def serverName = req.getServerName
        def serverPort = req.getServerPort
        lazy val url = req.getRequestURL.toString
        def basePathString = req.getContextPath
        def servicePathString = req.getServletPath
        def remainderString = fromNull(req.getPathInfo).getOrElse("")
        def baseUrl = wrapper.baseUrl
        def secureBaseUrl = wrapper.secureBaseUrl
        
        lazy val fileUploads = HashMap[String, Array[Byte]]()

        private def stripQuotes(s: String) = if(s.startsWith("\"")) s.substring(1, s.length - 1) else s

        val params = contentType match {
          case MimeTypes.`multipart/form-data` =>
            val params = new ListBuffer[Request.QueryParam]
            val ct = headers("Content-Type").head.split("; *")
            val boundary = ct.find(_.startsWith("boundary=")).get.substring(9)
            val mpr = new MultipartReader(boundary, req.getInputStream)
            while(mpr.ready()) {
              mpr.read() foreach { m =>
                if(m.filename.isDefined) {
                  fileUploads(stripQuotes(m.name.get)) = m.data
                  params += Request.StringQueryParam(stripQuotes(m.name.get), stripQuotes(m.filename.get))
                } else params += new Request.StringQueryParam(m.name.get,
                    new String(m.data, fromNull(req.getCharacterEncoding).getOrElse("ASCII")))
              }
            }
            params.toList
          
          case MimeTypes.`application/x-www-form-urlencoded` =>
            val params = new ListBuffer[Request.QueryParam]
            val enum = req.getParameterNames
            while(enum.hasMoreElements) {
              val name = enum.nextElement.asInstanceOf[String]
              for(value <- req.getParameterValues(name))
                params += new Request.StringQueryParam(name, value)
            }
            params.toList
        }

        private val input = {
          val in = req.getInputStream
          implicit val impl = rapture.io.StringCharReader
          implicit val enc = Encodings.`UTF-8`
          inputStreamCharBuilder.input(in)
        }

        lazy val body =
          if(streamRead) throw new Exception("Stream has already been read")
          else {
            implicit val impl = rapture.io.StringCharReader
            implicit val ca = rapture.io.CharAccumulator
            implicit val enc = Encodings.`UTF-8`
            streamRead = true
            input.slurp()
          }

        lazy val headers = new scala.collection.immutable.HashMap[String, Seq[String]] {
          private def enum2list(e: java.util.Enumeration[_]) = {
            val lb = new ListBuffer[String]
            while(e.hasMoreElements) lb += e.nextElement.asInstanceOf[String]
            lb
          }
          override def get(key: String) = Some(enum2list(req.getHeaders(key)))
          override def size = enum2list(req.getHeaderNames).length
          override def iterator = enum2list(req.getHeaderNames).map(n => (n, enum2list(req.getHeaders(n)))).iterator
        }
      }
    } catch { case e: Exception =>
      rapture.io.log.warn("Failure during creation of Request object.")
      rapture.io.log.exception(e)
      throw e
    }

    val vResp = handle(vReq)

    resp.setStatus(vResp.code)
    for((n, v) <- vResp.headers) resp.addHeader(n, v)
    for(rc <- vResp.cookies) {
      val c = new Cookie(rc.name, rc.value)
      c.setDomain(rc.domain)
      c.setMaxAge(rc.expires match {
        case Some(t) => (t / 1000).asInstanceOf[Int]
        case None => -1
      })
      c.setPath(rc.path)
      c.setSecure(rc.secure)
      resp.addCookie(c)
    }

    val r = try {
      vResp match {
        case BufferResponse(_, _, _, ct, buffers) =>
          resp.setContentType(ct.name)
          val out = resp.getOutputStream
          var spare: ByteBuffer = null
          for(b <- buffers) {
            if(b.hasArray) {
              out.write(b.array, b.arrayOffset, b.limit)
            } else {
              if(spare == null) spare = ByteBuffer.allocate(65536)
              while(b.hasRemaining) {
                spare.clear()
                spare.put(b)
                out.write(spare.array, 0, spare.position)
              }
            }
          }
          out.flush()
        
        case sr@StreamResponse(_, _, _, ct, send) =>
          val enc = Encodings.`UTF-8`
          resp.setContentType(ct.name+"; charset="+enc.name)
          val w = new BufferedWriter(new OutputStreamWriter(resp.getOutputStream(), enc.name))
          ensuring(new CharOutput(w))(send)(_.close())
        
        case ErrorResponse(code, _, _, message, _) =>
          resp.sendError(code, message)
        
        case FileResponse(_, _, _, ct, file) =>
          resp.setContentType(ct.name)
          resp.setContentLength(file.length.toInt)
          val out = new ByteOutput(resp.getOutputStream)
          implicit val ca = rapture.io.FileStreamByteReader
          file > out
          out.flush()
        
        case RedirectResponse(_, _, location) =>
          resp.sendRedirect(location)
      }
    } catch { case e: Exception =>
      rapture.io.log.warn("Failure during response generation")
      rapture.io.log.exception(e)
      throw e
    }
    rapture.io.log.debug("Request handled in "+(System.currentTimeMillis - t0)+"ms")
    r
  }
}
