package rapture.web

import org.osgi.framework._
import org.osgi.util.tracker._
import scala.xml._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ListMap

import rapture.io._
import rapture.orm._
import rapture.interfaces._

case class InitializationException(subject : String, message : String) extends RuntimeException

trait Cloudlet extends Http {

  log.info("Starting cloudlet "+project+" on "+hostname)

  def captureDomain(domain : String, register : Boolean = false) =
    try raptureService.registerDomainName(domain) catch { case e : Exception =>
      throw new InitializationException("Unable to register domain name", "Message")
    }
}

trait HerokuDatabase { http : Http =>

  def tables : List[Table[R] forSome { type R <: Record }]

  def dbHost : String
  def dbUser : String
  def dbPassword : String
  def dbPort : Int
  def dbName : String

  def getDatabase(migrate : Boolean = false) : DbPool = {
    val pool = new PostgresDbPool(dbHost, dbName, dbUser, dbPassword, port = dbPort, ssl = true)
    pool.migrateSql(tables : _*) match {
      case Nil => log.debug("Database schema is consistent")
      case sql =>
        if(migrate) {
          for(cmd <- sql) pool.acquireFor { db => Db.exec(cmd)(db) }
        } else throw new InitializationException("Database does not match schema",
          sql.mkString("; "))
    }
    pool
  }

}

trait DatabaseService { http : Http =>

  def tables : List[Table[R] forSome { type R <: Record }]

  def getDatabase(server : String = "db.propensive.com", migrate : Boolean = false) : DbPool = {
    val pool = new PostgresDbPool(server, "db_"+raptureService.projectName,
        "op_"+raptureService.projectName, "pw_"+raptureService.projectName)
    pool.migrateSql(tables : _*) match {
      case Nil => log.debug("Database schema is consistent")
      case sql =>
        if(migrate) {
          for(cmd <- sql) pool.acquireFor { db => Db.exec(cmd)(db) }
        } else throw new InitializationException("Database does not match schema",
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
  private var handlers : List[PartialFunction[Request, Response]] = Nil
  private var _raptureService : RaptureService = null
  private def raptureService_=(rs : RaptureService) = _raptureService = rs
  def raptureService = _raptureService

  private val httpServlet = new ServletWrapper {
    def baseUrl = ""
    def secureBaseUrl = ""

    // FIXME: Is there a better way of doing this?
    def handle(r : Request) : Response = try {
      var result : Option[Response] = None
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
      case e : Throwable =>
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
  private def registerServlet()(implicit ctx : org.osgi.framework.BundleContext, log : LogService) = {
    import org.osgi.service.http._
    log.info("Registering servlet with Jetty")
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
      log.debug("Initializing cloudlet")
      _bundleContext = context
      raptureService = standalone.getOrElse(service[RaptureService](context, implicitly[Manifest[RaptureService]]).get)
      raptureService.setScaleFunction(scale)
      for(code <- initCode) code()
      for(code <- startTasks) code()
      service[org.osgi.service.http.HttpService] foreach { svc => log.info("svc = "+svc)  }
      registerServlet()(context, log)
      status = Live
      log.debug("Cloudlet active")
    } catch {
      case InitializationException(subject, message) =>
        raptureService.postNotice(subject, message)
      case e : Exception =>
        status = FailedToStart
        log.error("Cloudlet failed to start")
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

  private val nullSuite = new Suite("Tests")
  private var currentTestSuite : Suite = nullSuite
  private var testSuites : ListMap[String, Suite] = ListMap()
  
  def setUp(blk : () => Unit) = currentTestSuite.setUp = Some(blk)
  def tearDown(blk : () => Unit) = currentTestSuite.setUp = Some(blk)
  
  def suite(name : String)(blk : => Unit) = {
    val s = new Suite(name)
    currentTestSuite = s
    blk
    currentTestSuite = nullSuite
    testSuites(s.name) = s
  }

  def test[T](name : String)(blk : => T) = new TestDef[T](name, blk)
  
  abstract class Test[T](val name : String) {
    def run() : T
    def check(t : T) : Boolean
    def doCheck() : Boolean = check(run())
  }

  class Suite(val name : String) {
    var setUp : Option[() => Unit] = None
    val tests : ListMap[String, Test[_]] = ListMap()
    var tearDown : Option[() => Unit] = None
    def run() : Int = {
      var success = 0
      setUp.foreach(_())
      tests.toList.reverse foreach { case (tn, t) =>
        val tr = TestResult(tn, try Some(t.doCheck()) catch { case e : Throwable => None })
        val result = tr.success match {
          case Some(true) =>
            success += 1
            "[SUCCESS]"
          case Some(false) => "[FAILURE]"
          case None => "[ ERROR ]"
        }
        val desc = if(tn.length > 64) tn.substring(0, 64) else tn+(" "*(64 - tn.length))
        log.info("  "+desc+result)
      }
      tearDown.foreach(_())
      success
    }
  }

  class TestDef[T](name : String, blk : => T) {

    def satisfies(sat : T => Boolean) : Test[T] = {
      val t = new Test[T](name) {
        def run() : T = blk
        def check(t : T) : Boolean = sat(t)
      }
      currentTestSuite.tests(name) = t
      t
    }

    def throwsException() : Test[Boolean] = {
      val t = new Test[Boolean](name) {
        def run() : Boolean = try { blk; false } catch { case e : Exception => true }
        def check(t : Boolean) : Boolean = t
      }
      currentTestSuite.tests(name) = t
      t
    }

    def yields(y : => T) : Test[T] = {
      val t = new Test[T](name) {
        def run() : T = blk
        def check(t : T) : Boolean = t == y
      }
      currentTestSuite.tests(name) = t
      t
    }
  }

  case class TestResult(name : String, success : Option[Boolean])

  def runAllSuites() = {
    nullSuite.run()
    testSuites.toList.reverse foreach { case (n, ts) =>
      log.info(n+":")
      ts.run()
    }
  }

  def listSuites() = testSuites.keys.toList
  def listTests(suite : String) = testSuites.get(suite).map(_.tests.map(_._2)).flatten

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
  def notFound(r : Request)(implicit log : LogService = NoLogging) : Response = {
    log.info("Not found: "+r.path)
    ErrorResponse(404, Nil, Nil, "Not found", "The requested resource could not be found")
  }

  /** A simple error response */
  def error(r : Request, e : Throwable)(implicit log : LogService = NoLogging) : Response = {
    log.error("Exception: "+e.getMessage)
    e.getStackTrace foreach { ste => log.error("    "+ste) }
    ErrorResponse(500, Nil, Nil, "An unexpected error has occurred", "Unknown error")
  }

  /** A standard implementaiton of a response which confirms cross-domain access corntrol */
  def accessControlAllowOrigin(domain : String)(implicit enc : Encodings.Encoding) : Response =
    StreamResponse(200, ("Access-Control-Allow-Origin" -> domain) :: ("Access-Control-Allow-Credentials" -> "true") :: Response.NoCache, Nil, MimeTypes.`application/xml`, v => ())

}

trait Handlers {

  trait Handler[-T] { def response(t : T) : Response }

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

  implicit def htmlHandler(implicit enc : Encodings.Encoding) = new Handler[Html] {
    def response(h : Html) = StreamResponse(200, Response.NoCache, Nil,
        MimeTypes.`text/html`, { os =>
      h.doctype.declaration pumpTo os
      h.content.toString pumpTo os
    })
  }

  implicit def charInputHandler(implicit enc : Encodings.Encoding) = new Handler[Input[Char]] {
    def response(in : Input[Char]) = StreamResponse(200, Response.NoCache, Nil,
        MimeTypes.`text/plain`, { os =>
      in.pumpTo(os)
      os.close()
    })
  }

  implicit def xmlHandler(implicit enc : Encodings.Encoding) = new Handler[Seq[Node]] {
    def response(t : Seq[Node]) = StreamResponse(200, Response.NoCache, Nil,
        MimeTypes.`application/xml`, { os =>
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" pumpTo os
      t.toString pumpTo os
      os.close()
    })
  }
  implicit def stringHandler(implicit enc : Encodings.Encoding) = new Handler[String] {
    def response(t : String) = StreamResponse(200, Response.NoCache, Nil,
        MimeTypes.`text/plain`, { os =>
      t pumpTo os
      os.close()
    })
  }
  implicit val redirectHandler = new Handler[NetUrl[_]] {
    def response(path : NetUrl[_]) = RedirectResponse(Nil, Nil, path.toString)
  }

  implicit def fileHandler = new Handler[FileUrl] {
    def response(file : FileUrl) = FileResponse(200, Response.NoCache, Nil,
        file.extension.toList.flatMap(MimeTypes.extension).headOption.getOrElse(
	      MimeTypes.`text/plain`), file)
  }

  import util.parsing.json._

  implicit def jsonArrayHandler(implicit enc : Encodings.Encoding) = new Handler[JSONArray] {
    def response(t : JSONArray) = StreamResponse(200, Response.NoCache, Nil,
        MimeTypes.`application/json`, { os =>
      t.toString() pumpTo os
      os.close()
    })
  }
  
  implicit def jsonObjectHandler(implicit enc : Encodings.Encoding) = new Handler[JSONObject] {
    def response(t : JSONObject) = StreamResponse(200, Response.NoCache, Nil,
        MimeTypes.`application/json`, { os =>
      t.toString() pumpTo os
      os.close()
    })
  }

}
