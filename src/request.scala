package rapture.web

import language._

import scala.collection.mutable.HashMap
import java.io._
import rapture.io._

object Request {
  sealed trait QueryParam { def name: String }
  case class StringQueryParam(name: String, value: String) extends QueryParam
  case class XmlQueryParam(value: scala.xml.Node) extends QueryParam { def name = "XML" }
  case class FileQueryParam(name: String, localFile: File)(val clientFilename: String, val contentType: MimeTypes.MimeType) extends QueryParam

}

/** Represents an HTTP request. */
abstract class Request {

  /** The length of incoming data, e.g. for POST or PUT. */
  def contentLength: Int

  /** The part of the URI following the ?, or the empty string. */
  def queryString: String

  /** The HTTP method, e.g. GET or POST, etc. */
  def requestMethod: HttpMethods.Method

  /** The virtual path, i.e. the part of the URL following the hostname, not
   *  including any query parameters. Always starts with a slash. */
  def scriptName: String

  /** If true, indicates that this is a secure connection. */
  def https: Boolean

  /** The name the web server thinks this (virtual) server is called, e.g.
   *  www.example.com. */
  def serverName: String

  /** The port the web server says the request was made to. */
  def serverPort: Int

  /** The requested URL, without any query parameters, e.g.
   *  http://www.example.com:8080/basePath/servicePath/remainder. */
  def url: String

  /** The application base path used in this request, relative to the domain
   * root. Conventionally has a leading slash only, unless it's empty. */
  def basePathString: String

  /** The service path requested, relative to the base path. Conventionally
   *  has a leading slash only, unless it's empty. */
  def servicePathString: String

  /** The remainder of the URL following the service path, without any query
   *  parameters. Conventionally has a leading slash only, unless it's empty. */
  def remainderString: String

  /** The canonical deployment base URL of this application, e.g.
   *  http://www.example.com:8080/basePath. */
  def baseUrl: String

  /** The canonical secure deployment base URL, e.g.
   *  https://secure.example.com/basePath. */
  def secureBaseUrl: String

  /** Array of all query and POST parameters in order. */
  def params: Seq[Request.QueryParam]

  def fileUploads: Map[String, Array[Byte]]

  /** Request headers. */
  def headers: Map[String, Seq[String]]

  /** Body PUT or POSTed */
  def body: String

  /** The time the request arrived */
  val time: Long = System.currentTimeMillis

  /** The path of the script */
  lazy val path: SimplePath = new SimplePath((servicePathString+remainderString).replaceAll("^\\/", "").split("\\/").filter(_ != ""), "") {
    val params = Nil
  }

  protected var streamRead = false

  lazy val remainder: List[String] = remainderString.split("\\/").toList

  private lazy val pmap: HashMap[String, String] = {
    val pm = new HashMap[String, String]
    params.foreach {
      case Request.StringQueryParam(k, v) => pm(k) = v
      case _ => ()
    }
    pm
  }

  lazy val parameterMap: Map[String, String] = pmap.toMap

  /** Checks for the existence of a named request param. */
  def exists(k: Symbol): Boolean = pmap.contains(k.name)

  /** Gets a named request param (which must exist). */
  def apply(k: String): String = pmap.get(k) match {
    case Some(v) => v
    case None => throw new Exception("Missing parameter: "+k)
  }

  /** Gets a named request param or returns the default. */
  def param(k: String, default: String): String = pmap.get(k) match {
    case Some(v) => v
    case None => default
  }

  /** Gets a named request param which may not exist. */
  def param(k: String): Option[String] = pmap.get(k)

  /*private var xmlSet = false
  private var _xml: Option[Node] = None
  /** Possible POSTed XML. */
  def xml: Option[Node] = {
    if(!xmlSet) {
      params.foreach {
        case Request.XmlQueryParam(x) => _xml = Some(x)
        case _ => ()
      }
      xmlSet = true
    }
    _xml
  }*/

  /** Gets the value of a cookie from the request */
  def cookie(c: String) = cookies.get(c)

  private lazy val cookies: scala.collection.immutable.Map[String, String] = {
    var cs = scala.collection.immutable.Map[String, String]()
    headers.get("cookie") match {
      case Some(Seq(v)) =>
      val vs = v.split("; ")
      for(v <- vs) {
        val kv = v.split("=", 2) 
        if(kv.length == 2) cs = cs + (kv(0).urlDecode -> kv(1).urlDecode)
      }
      case _ => ()
    }
    cs
  }
}
