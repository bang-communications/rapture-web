package rapture.web

import org.osgi.framework._
import org.osgi.util.tracker._
import scala.xml._
import scala.collection.mutable.ListBuffer

import rapture.io._
import rapture.orm._


case class InitializationException(subject : String, message : String) extends RuntimeException

trait Cloudlet extends Http {

  log.info("Starting cloudlet "+project+" on "+hostname)

  def captureDomain(domain : String, register : Boolean = false) =
    try raptureService.registerDomainName(domain) catch { case e : Exception =>
      throw new InitializationException("Unable to register domain name", "Message")
    }
}

trait DatabaseService { http : Http =>

  def tables : List[Table[R] forSome { type R <: Record }]

  def getDatabase() : DbPool = {
    val pool = new PostgresDbPool("db.propensive.com", "db_"+raptureService.projectName,
        "op_"+raptureService.projectName, "pw_"+raptureService.projectName)
    pool.migrateSql(tables : _*) match {
      case Nil => log.debug("Database schema is consistent")
      case sql => throw new InitializationException("Database does not match schema",
          sql.mkString("; "))
    }
    pool
  }

}

/** This trait provides a nice interface to the HTTP server */
trait Http extends DelayedInit with Handlers with BundleActivator { main : BundleActivator =>

  import Osgi._

  def startTask(block : => Unit) =
    startTasks.append(() => block)
  
  def stopTask(block : => Unit) =
    stopTasks.append(() => block)

  private var initCode : Option[() => Unit] = None
  private[web] val stopTasks = new ListBuffer[() => Unit]
  private[web] val startTasks = new ListBuffer[() => Unit]

  protected val hostname = scala.sys.process.Process("/bin/hostname").!!.trim
  protected var project : Option[String] = None
  protected var status : Status = Waiting



  private var _bundleContext : BundleContext = _
  private var trackedService : Option[ServiceTracker] = None
  private var handlers : List[PartialFunction[Request, HttpResponse]] = Nil
  private var _raptureService : RaptureService = null
  private def raptureService_=(rs : RaptureService) = _raptureService = rs
  def raptureService = _raptureService

  private val httpServlet = new ServletWrapper {
    def baseUrl = ""
    def secureBaseUrl = ""

    // FIXME: Is there a better way of doing this?
    def handle(r : Request) : HttpResponse = try {
      var result : Option[HttpResponse] = None
      /*handlers.reverse find { pf =>
        result = pf.lift(r)
        result.isDefined
      }
      result.getOrElse(notFound(r))*/

      val hs = handlers.reverse.iterator
      while(hs.hasNext && result.isEmpty) {
        try { result = Some(hs.next()(r)) } catch { case e : MatchError => () }
      }
      result.getOrElse(notFound(r))
    } catch {
      case e : Exception =>
        error(r, e)
    }
  }
 
  def nodeName = raptureService.nodeName

  private def unregisterServlet()(implicit ctx : org.osgi.framework.BundleContext, log : LogService = NoLogging) = {
    trackedService.foreach(_.close())
    log.info("Stopped tracking services")
  }

  /** Registers the servlet with the HTTP service when it becomes available, and unregisters it
    * when it ceases to be available */
  private def registerServlet()(implicit ctx : org.osgi.framework.BundleContext, log : LogService = NoLogging) = {
    import org.osgi.service.http._
    trackedService = Some(trackService[HttpService] {
      case Add(httpService) =>
        log.info("HTTP Service added")
        httpService.registerServlet("/", httpServlet, null, null)
      case Change(_) =>
        log.info("HTTP Service changed")
      case Remove(httpService) =>
        log.info("HTTP Service removed")
        httpService.unregister("/")
    })
  }

  implicit val log : LogService = new FileLogger(File / "var" / "log" / "rapture")
  implicit final def bundleContext : BundleContext = _bundleContext

  override final def delayedInit(body: => Unit) = initCode = Some(() => body)
 
  def standalone : Option[RaptureService] = None

  final def start(context : BundleContext) = {
    try {
      status = Initializing
      log.debug("Cloudlet status = initializing")
      _bundleContext = context
      raptureService = standalone.getOrElse(service[RaptureService](context, implicitly[Manifest[RaptureService]]).get)
      raptureService.setScaleFunction(scale)
      for(code <- initCode) code()
      for(code <- startTasks) code()
      registerServlet()(context)
      status = Live
      log.debug("Cloudlet status = live")
    } catch {
      case InitializationException(subject, message) =>
        raptureService.postNotice(subject, message)
      case e : Exception =>
        status = FailedToStart
        log.error("Cloudlet status = failed to start")
        log.exception(e)
    }
  }

  final def stop(context : BundleContext) = {
    _bundleContext = null
    for(action <- stopTasks) action()
    unregisterServlet()(context)
  }
  
  def scale(avgs : List[(Double, Double, Double)]) : Int = {
    val avg = avgs.foldLeft(0.0)(_ + _._3)/avgs.length
    val n = if(avg >= 1.0) (avgs.length + 1)
    else if(avg <= (1.0 - 1.0/avgs.length)) (avgs.length - 1)
    else avgs.length
    List(List(n, maxNodes).min, minNodes).max
  }

  def maxNodes = 5
  def minNodes = 1

  /** Method to allow registration of handlers for requests matching different conditions */
  def handle[T](fn : PartialFunction[Request, T])(implicit handler : Handler[T], log : LogService) =
    handlers ::= { fn andThen handler.response }


  /** Provides a choice of options for how to match URL paths */
  object PathMatching extends Enumeration {
    val NoTrailingSlash = Value("No trailing slashes")
    val RequireTrailingSlash = Value("Require trailing slashes")
    val IgnoreTrailingSlash = Value("Ignore trailing slashes")
  }

  /** Options regarding how pattern matching should work on requests */
  trait RequestOptions {
    def httpMethods : List[Request.Method]
    def pathMatching : PathMatching.Value
    def validate(r : Request) =
      httpMethods.contains(r.requestMethod) && (pathMatching match {
        case PathMatching.NoTrailingSlash => r.url.last != '/'
        case PathMatching.RequireTrailingSlash => r.url.last == '/'
        case PathMatching.IgnoreTrailingSlash => true
      })
  }

  /** Provide a default implementation of RequestOptions */
  implicit object DefaultRequestOptions extends RequestOptions {
    def httpMethods : List[Request.Method] = List(Request.Get, Request.Post)
    def pathMatching = PathMatching.IgnoreTrailingSlash
  }

  /** Extract the path from the request */
  object Path { def unapply(r : Request) : Option[rapture.io.Path] = Some(r.path) }

  /** Defines a pattern matching construct to be used to chain together constraints on requests */
  object & { def unapply(r : Request) = Some((r, r)) }

  /** Method for creating new parameter extractors for requests */
  def withParam(p : String) = new { def unapply(r : Request) : Option[String] = r.param(p) }

  /** Method for producing new cookie extractors for requests */
  def withCookie(c : String) = new {
    def unapply(r : Request)(implicit log : LogService = NoLogging) : Option[String] = {
      val res = r.cookie(c)
      //log.debug("With Cookie "+c+": "+res)
      res
    }
  }

  /** Method for creating new HTTP header extractors for requests */
  def withHttpHeader(h : String) = new {
    def unapply(r : Request)(implicit log : LogService = NoLogging) : Option[String] = {
      val res = r.headers.get(h).flatMap(_.headOption)
      log.debug("With HTTP header "+h+": "+res)
      res
    }
  }

  /*8 A simple "Not found" page */
  def notFound(r : Request)(implicit log : LogService = NoLogging) : HttpResponse = {
    log.info("Not found: "+r.path)
    ErrorResponse(404, Nil, Nil, "Not found", "The requested resource could not be found")
  }

  /** A simple error response */
  def error(r : Request, e : Exception)(implicit log : LogService = NoLogging) : HttpResponse = {
    log.error("Exception: "+e.getMessage)
    e.getStackTrace foreach { ste => log.error("    "+ste) }
    ErrorResponse(500, Nil, Nil, "An unexpected error has occurred", "Unknown error")
  }

  /** A standard implementaiton of a response which confirms cross-domain access corntrol */
  def accessControlAllowOrigin(domain : String) : HttpResponse =
    StreamResponse(200, ("Access-Control-Allow-Origin" -> domain) :: ("Access-Control-Allow-Credentials" -> "true") :: HttpResponse.NoCache, Nil, MimeTypes.`application/xml`, v => ())

}

trait Handlers {

  trait Handler[-T] { def response(t : T) : HttpResponse }

  case class Doctype(declaration : String)

  object Html4Strict extends Doctype("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">")
  object Html4Transitional extends Doctype("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">")
  object Html4Frameset extends Doctype("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" \"http://www.w3.org/TR/html4/frameset.dtd\">")
  object Xhtml1Strict extends Doctype("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
  object Xhtml1Transitional extends Doctype("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")
  object Xhtml1Frameset extends Doctype("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">")
  object Html32 extends Doctype("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">")
  object Html2 extends Doctype("<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">")

  case class Html(content : Seq[Node], doctype : Doctype = Xhtml1Strict)

  implicit val htmlHandler = new Handler[Html] {
    def response(h : Html) = StreamResponse(200, HttpResponse.NoCache, Nil,
        MimeTypes.`text/html`, { os =>
      h.doctype.declaration pumpTo os
      h.content.toString pumpTo os
    })
  }

  implicit val charInputHandler = new Handler[Input[Char]] {
    def response(in : Input[Char]) = StreamResponse(200, HttpResponse.NoCache, Nil,
        MimeTypes.`text/plain`, { os =>
      in.pumpTo(os)
      os.close()
    })
  }

  implicit val xmlHandler = new Handler[Seq[Node]] {
    def response(t : Seq[Node]) = StreamResponse(200, HttpResponse.NoCache, Nil,
        MimeTypes.`application/xml`, { os =>
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" pumpTo os
      t.toString pumpTo os
      os.close()
    })
  }
  implicit val stringHandler = new Handler[String] {
    def response(t : String) = StreamResponse(200, HttpResponse.NoCache, Nil,
        MimeTypes.`text/plain`, { os =>
      t pumpTo os
      os.close()
    })
  }
  implicit val redirectHandler = new Handler[NetUrl[_]] {
    def response(path : NetUrl[_]) = RedirectResponse(Nil, Nil, path.toString)
  }

  implicit val fileHandler = new Handler[FileUrl] {
    def response(file : FileUrl) = FileResponse(200, HttpResponse.NoCache, Nil,
        file.extension.toList.flatMap(MimeTypes.extension).headOption.getOrElse(
	      MimeTypes.`text/plain`), file)
  }

  import util.parsing.json._

  implicit val jsonArrayHandler = new Handler[JSONArray] {
    def response(t : JSONArray) = StreamResponse(200, HttpResponse.NoCache, Nil,
        MimeTypes.`application/json`, { os =>
      t.toString() pumpTo os
      os.close()
    })
  }
  
  implicit val jsonObjectHandler = new Handler[JSONObject] {
    def response(t : JSONObject) = StreamResponse(200, HttpResponse.NoCache, Nil,
        MimeTypes.`application/json`, { os =>
      t.toString() pumpTo os
      os.close()
    })
  }

}
