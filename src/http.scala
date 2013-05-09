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

import javax.servlet.http._

import rapture.io._

case class InitializationException(subject: String, message: String) extends RuntimeException

trait BasicRequests { this: HttpServer =>
  type WebRequest = BasicRequest
  def makeRequest(req: HttpServletRequest, resp: HttpServletResponse) = new BasicRequest(req, resp)
}

trait StandaloneHttpServer extends HttpServer {

  import org.eclipse.jetty.server.Server
  import org.eclipse.jetty.servlet.ServletContextHandler
  import org.eclipse.jetty.servlet.ServletHolder

  def serverPort = 80

  val server = new Server(serverPort)
  val sch = new ServletContextHandler(server, "/", true, false)
  
  implicit private val zone2 = Zone("init")
  
  final def start() = {
    try {
      for(action <- initCode) action()
      for(action <- startTasks) action()
      sch.addServlet(new ServletHolder(httpServlet), "/")
      server.start()
    } catch {
      case e: Exception =>
        log.error("Failed to start: ")
        log.exception(e)
    }
  }

  final def stop() = for(action <- stopTasks) action()

}

/** This trait provides a nice interface to the HTTP server */
trait HttpServer extends DelayedInit with Servlets with RequestHandlers with RequestExtractors {

  type WebRequest <: Request

  implicit private val zone = Zone("web")
  
  def makeRequest(req: HttpServletRequest, resp: HttpServletResponse): WebRequest

  def onStart(block: => Unit) =
    startTasks.append(() => block)
  
  def onShutdown(block: => Unit) =
    stopTasks.append(() => block)

  var initCode: Option[() => Unit] = None
  protected[web] val stopTasks = new ListBuffer[() => Unit]
  protected[web] val startTasks = new ListBuffer[() => Unit]

  protected var handlers: List[PartialFunction[WebRequest, Response]] = Nil
  protected var errorHandler: Option[PartialFunction[(WebRequest, Throwable), Response]] = None
  protected var notFoundHandler: Option[PartialFunction[WebRequest, Response]] = None

  protected val httpServlet = new HttpServletWrapper
  
  class HttpServletWrapper extends ServletWrapper {

    /** FIXME: This is not tail-recursive. */
    def handle(r: WebRequest): Response = try {
      yCombinator[(List[PartialFunction[WebRequest, Response]], WebRequest), Response] { f =>
        _._1 match {
          case Nil => notFoundHandler.map(_(r)).getOrElse(notFound(r))
          case h :: t => h.applyOrElse(r, (g: WebRequest) => f(t -> g))
        }
      } (handlers -> r)
    } catch { case e: Throwable => error(r, e) }
  }

  override final def delayedInit(body: => Unit) = initCode = Some(() => body)
 
  /** Method to allow registration of handlers for requests matching different conditions */
  def handle[T](fn: PartialFunction[WebRequest, T])(implicit handler: Handler[T]) =
    handlers ::= { fn andThen handler.response }

  def respond[T](t: T)(implicit handler: Handler[T]) = handler.response(t)

  implicit class FormExtras[F <: Forms.WebForm](f: F) {
    def show[T: Handler](p1: F => T) = new {
      def andThenGoto[S: Handler](s: S): Response =
        if(f.complete) {
          f.save()
          implicitly[Handler[S]].response(s)
        } else implicitly[Handler[T]].response(p1(f))

      def andThen[S: Handler](p2: F => S): Response =
        if(f.complete) {
          f.save()
          implicitly[Handler[S]].response(p2(f))
        } else implicitly[Handler[T]].response(p1(f))
    }
  }

  def handleError[T](fn: PartialFunction[(WebRequest, Throwable), T])(implicit handler: Handler[T]) =
    errorHandler = Some({ fn andThen handler.response })

  def handleNotFound[T](fn: PartialFunction[WebRequest, T])(implicit handler: Handler[T]) =
    notFoundHandler = Some({ fn andThen handler.response })

  /** A simple "Not found" page */
  def notFound(r: WebRequest): Response = {
    //log.info("Not found: "+r.path)
    ErrorResponse(404, Nil, "Not found", "The requested resource could not be found")
  }

  /** A simple error response */
  def error(r: WebRequest, e: Throwable): Response = {
    log.error(e.getMessage)
    log.exception(e)
    try {
      errorHandler.map(_(r, e)) getOrElse {
        ErrorResponse(500, Nil, "An unexpected error has occurred", "Unknown error")
      }
    } catch { case e: Exception =>
      log.error("Further error occurred whilst handling error page: "+e.getMessage)
      log.exception(e)
      ErrorResponse(500, Nil, "An unexpected error has occurred", "Unknown error")
    }
  }
}
