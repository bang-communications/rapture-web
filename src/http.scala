package rapture.web

import org.osgi.framework._
import org.osgi.util.tracker._
import scala.xml._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ListMap

import javax.servlet.http._

import rapture.io._
import rapture.html._

case class InitializationException(subject: String, message: String) extends RuntimeException

trait BasicRequests { this: HttpServer =>
  type WebRequest = BasicRequest
  def makeRequest(req: HttpServletRequest, resp: HttpServletResponse) = new BasicRequest(req, resp)
}

/** This trait provides a nice interface to the HTTP server */
trait HttpServer extends DelayedInit with BundleActivator with Handlers with Servlets {

  type WebRequest <: Request

  implicit val zone = Zone("web")

  def makeRequest(req: HttpServletRequest, resp: HttpServletResponse): WebRequest

  import Osgi._

  def onStart(block: => Unit) =
    startTasks.append(() => block)
  
  def onShutdown(block: => Unit) =
    stopTasks.append(() => block)

  var initCode: Option[() => Unit] = None
  private[web] val stopTasks = new ListBuffer[() => Unit]
  private[web] val startTasks = new ListBuffer[() => Unit]

  private var _bundleContext: BundleContext = _
  private var trackedService: Option[ServiceTracker] = None
  private var handlers: List[PartialFunction[WebRequest, Response]] = Nil
  private var errorHandler: Option[PartialFunction[(WebRequest, Throwable), Response]] = None
  private var notFoundHandler: Option[PartialFunction[WebRequest, Response]] = None

  private val httpServlet = new HttpServletWrapper
  
  class HttpServletWrapper extends ServletWrapper {

    def handle(r: WebRequest): Response = try {
      var result: Option[Response] = None

      val hs = handlers.reverse.iterator
      while(hs.hasNext && result.isEmpty) {
        try { result = Some(hs.next()(r)) } catch {
          case e: MatchError =>
            /* We need to be able to distinguish between a MatchError thrown because no handler
             * was found, and a MatchError thrown due to user code. We do this be inspecting the
             * stack trace. It's an ugly solution, but there's no other convenient way I can think
             * of. */
            val ignores = List(
              "scala.",
              "rapture.web.",
              "javax.servlet.http.",
              "org.apache.felix.",
              "javax.servlet.",
              "org.mortbay.jetty.",
              "org.mortbay.io.",
              "org.mortbay.thread."
            )
            
            if(e.getStackTrace exists { f => 
              ignores forall { s => !f.getClassName.startsWith(s) }
            }) result = Some(error(r, e))
        }
      }
      result.orElse(notFoundHandler.map(_(r))).getOrElse(notFound(r))
    } catch {
      case e: Throwable =>
        error(r, e)
    }
  }
 
  private def unregisterServlet() = {
    trackedService.foreach(_.close())
    //log.info("Stopped tracking services")
  }

  /** Registers the servlet with the HTTP service when it becomes available, and unregisters it
    * when it ceases to be available */
  private def registerServlet() = {
    import org.osgi.service.http._
    trackedService = Some(trackService[HttpService] {
      case Add(httpService) =>
        httpService.registerServlet("/", httpServlet, null, null)
      case Change(_) =>
      case Remove(httpService) =>
        httpService.unregister("/")
    })
  }

  implicit final def bundleContext: BundleContext = _bundleContext

  override final def delayedInit(body: => Unit) = initCode = Some(() => body)
 
  final def start(context: BundleContext) = {
    try {
      _bundleContext = context
      for(code <- initCode) code()
      for(code <- startTasks) code()
      registerServlet()
    } catch {
      case e: Exception =>
        log.error("Failed to start: ")
        log.exception(e)
    }
  }

  final def stop(context: BundleContext) = {
    _bundleContext = null
    for(action <- stopTasks) action()
    unregisterServlet()
  }
  
  /** Method to allow registration of handlers for requests matching different conditions */
  def handle[T](fn: PartialFunction[WebRequest, T])(implicit handler: Handler[T]) =
    handlers ::= { fn andThen handler.response }

  def respond[T](t: T)(implicit handler: Handler[T]) = handler.response(t)

  implicit def formExtras[F <: Forms.WebForm](f: F) = new {
    def show[T: Handler](p1: F => T) = new {
      def thenGoto[S: Handler](s: S): Response =
        if(f.complete) {
          f.save()
          implicitly[Handler[S]].response(s)
        } else implicitly[Handler[T]].response(p1(f))

      def then[S: Handler](p2: F => S): Response =
        if(f.complete) {
          f.save()
          implicitly[Handler[S]].response(p2(f))
        } else implicitly[Handler[T]].response(p1(f))
    }
  }

  def handleError[T](fn: PartialFunction[(WebRequest, Throwable), T])(implicit handler: Handler[T]) = {
    errorHandler = Some({ fn andThen handler.response })
  }

  def handleNotFound[T](fn: PartialFunction[WebRequest, T])(implicit handler: Handler[T]) = {
    notFoundHandler = Some({ fn andThen handler.response })
  }

  /** Provides a choice of options for how to match URL paths */
  object PathMatching extends Enumeration {
    val NoTrailingSlash = Value("No trailing slashes")
    val RequireTrailingSlash = Value("Require trailing slashes")
    val IgnoreTrailingSlash = Value("Ignore trailing slashes")
  }

  /** Options regarding how pattern matching should work on requests */
  trait RequestOptions {
    def httpMethods: List[HttpMethods.Method]
    def pathMatching: PathMatching.Value
    def validate(r: WebRequest) =
      httpMethods.contains(r.requestMethod) && (pathMatching match {
        case PathMatching.NoTrailingSlash => r.url.last != '/'
        case PathMatching.RequireTrailingSlash => r.url.last == '/'
        case PathMatching.IgnoreTrailingSlash => true
      })
  }

  /** Provide a default implementation of RequestOptions */
  implicit object DefaultRequestOptions extends RequestOptions {
    def httpMethods: List[HttpMethods.Method] = List(HttpMethods.Get, HttpMethods.Post)
    def pathMatching = PathMatching.IgnoreTrailingSlash
  }

  //trait ExtractorParser[T] { def parse(s: String): Option[T] }
  //implicit val IntExtractorParser = new ExtractorParser[Int] { def parse(s: String): Option[Int] = try Some(s.toInt) catch { case e: Exception => None } }

  //object As { def unapply[T](s: String)(implicit parser: ExtractorParser[T]): Option[T] = parser.parse(s) }

  object AsInt { def unapply(s: String): Option[Int] = try Some(s.toInt) catch { case e: Exception => None } }

  /** Extract the path from the request */
  object Path { def unapply(r: WebRequest): Option[rapture.io.Path] = Some(r.path) }

  /** Defines a pattern matching construct to be used to chain together constraints on requests */
  object & { def unapply(r: WebRequest) = Some((r, r)) }

  /** Method for creating new parameter extractors for requests */
  def withParam(p: Symbol) = new Param(p)

  class Param(p: Symbol) { def unapply(r: WebRequest): Option[String] = r.param(p) }

  class CookieExtractor(p: Symbol) { def unapply(r: WebRequest): Option[String] = r.cookie(p.name) }

  /** Method for producing new cookie extractors for requests */
  def withCookie(c: Symbol) = new CookieExtractor(c)

  class HttpHeader(p: String) { def unapply(r: WebRequest): Option[String] = r.headers.get(p).flatMap(_.headOption) }

  /** Method for creating new HTTP header extractors for requests */
  def withHttpHeader(h: String) = new HttpHeader(h)

  /*8 A simple "Not found" page */
  def notFound(r: WebRequest): Response = {
    //log.info("Not found: "+r.path)
    ErrorResponse(404, Nil, Nil, "Not found", "The requested resource could not be found")
  }

  /** A simple error response */
  def error(r: WebRequest, e: Throwable): Response = {
    log.error(e.getMessage)
    log.exception(e)
    try {
      errorHandler.map(_(r, e)) getOrElse {
        ErrorResponse(500, Nil, Nil, "An unexpected error has occurred", "Unknown error")
      }
    } catch { case e: Exception =>
      log.error("Further error occurred whilst handling error page: "+e.getMessage)
      log.exception(e)
      ErrorResponse(500, Nil, Nil, "An unexpected error has occurred", "Unknown error")
    }
  }

  /** A standard implementaiton of a response which confirms cross-domain access corntrol */
  def accessControlAllowOrigin(domain: String)(implicit enc: Encodings.Encoding): Response =
    StreamResponse(200, ("Access-Control-Allow-Origin" -> domain) :: ("Access-Control-Allow-Credentials" -> "true") :: Response.NoCache, Nil, MimeTypes.`application/xml`, v => ())(enc)


}

trait Handlers { this: HttpServer =>

  trait Handler[-T] { def response(t: T): Response }

  case class Doctype(declaration: String)

  object Html4Strict extends Doctype("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">")
  object Html4Transitional extends Doctype("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">")
  object Html4Frameset extends Doctype("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" \"http://www.w3.org/TR/html4/frameset.dtd\">")
  object Xhtml1Strict extends Doctype("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
  object Xhtml1Transitional extends Doctype("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")
  object Xhtml1Frameset extends Doctype("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">")
  object Html32 extends Doctype("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">")
  object Html2 extends Doctype("<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">")

  implicit def charInputHandler(implicit enc: Encodings.Encoding) = new Handler[Input[Char]] {
    def response(in: Input[Char]) = StreamResponse(200, Response.NoCache, Nil,
        MimeTypes.`text/plain`, { os =>
      in > os
      os.close()
    })
  }

  implicit def xmlHandler(implicit enc: Encodings.Encoding) = new Handler[Seq[Node]] {
    def response(t: Seq[Node]) = StreamResponse(200, Response.NoCache, Nil,
        MimeTypes.`application/xml`, { os =>
      StringInput("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n") > os
      StringInput(t.toString) > os
      os.close()
    })
  }
  implicit def stringHandler(implicit enc: Encodings.Encoding) = new Handler[String] {
    def response(t: String) = StreamResponse(200, Response.NoCache, Nil,
        MimeTypes.`text/plain`, { os =>
      StringInput(t) > os
      os.close()
    })
  }
  
  implicit val pathRedirectHandler = new Handler[Path] {
    def response(path: Path) = RedirectResponse(Nil, Nil, path.toString)
  }

  implicit def fileHandler = new Handler[FileUrl] {
    def response(file: FileUrl) = FileResponse(200, Response.NoCache, Nil,
        file.extension.toList.flatMap(MimeTypes.extension).headOption.getOrElse(
	      MimeTypes.`text/plain`), file)
  }

  import util.parsing.json._

  implicit val nullHandler = new Handler[Response] {
    def response(r: Response) = r
  }

  implicit def jsonArrayHandler(implicit enc: Encodings.Encoding) = new Handler[JSONArray] {
    def response(t: JSONArray) = StreamResponse(200, Response.NoCache, Nil,
        MimeTypes.`application/json`, { os =>
      StringInput(t.toString()) > os
      os.close()
    })
  }
  
  implicit def jsonObjectHandler(implicit enc: Encodings.Encoding) = new Handler[JSONObject] {
    def response(t: JSONObject) = StreamResponse(200, Response.NoCache, Nil,
        MimeTypes.`application/json`, { os =>
      StringInput(t.toString()) > os
      os.close()
    })
  }

  implicit def pageHandler(implicit enc: Encodings.Encoding) = new Handler[Layout.Page] {
    def response(page: Layout.Page) = StreamResponse(page.httpStatus, Response.NoCache, Nil,
        MimeTypes.`text/html`, { os =>
      page.stream > os
    })
  }

}
