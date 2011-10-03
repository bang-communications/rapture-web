package rapture.web

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.StringWriter

import org.jclouds.cloudservers._
import org.jclouds.compute._
import org.jclouds.blobstore._

import scala.collection.JavaConversions._

import org.jclouds.cloudservers.options._
import org.jclouds.cloudservers.domain._
import CreateServerOptions.Builder._
import ListOptions.Builder._

import rapture.web._
import rapture.io._


object CloudPaths extends LowPriorityCloudPaths {

  implicit object CloudFilesStreamByteReader extends StreamReader[CloudFilesUrl, Byte] {
    def input(url : CloudFilesUrl) : Input[Byte] =
      new ByteInput(url.urlBase.asInstanceOf[CloudFilesUrlBase].blobStore.get(
          url.elements.mkString("/")))
  }

  implicit object CloudFilesStreamByteWriter extends StreamWriter[CloudFilesUrl, Byte] {
    def output(url : CloudFilesUrl, append : Boolean = false) : Output[Byte] = {
      val baos = new ByteArrayOutputStream
      new ByteOutput(baos) {
        override def flush() = ()
        override def close() = {
          baos.close()
          val ub = url.urlBase.asInstanceOf[CloudFilesUrlBase]
          val bc = ub.cloudContext
          val bs = bc.createInputStreamMap(ub.container)
          bs.putBytes(url.elements.mkString("/"), baos.toByteArray)
          bc.close()
        }
      }
    }
  }
}

trait LowPriorityCloudPaths {

  abstract class CloudFilesUrl extends Url[CloudFilesUrl] with PathUrl[CloudFilesUrl] {
    def makePath(elems : Seq[String]) = urlBase.makePath(elems)
  }

  object CloudFiles extends Scheme[CloudFilesUrl]("cloudfiles") {
    def /(container : String)(implicit cloud : Cloud) = new CloudFilesUrlBase(container)
  }

  class CloudFilesUrlBase(val container : String)(implicit cloud : Cloud)
      extends UrlBase[CloudFilesUrl] { thisUrlBase =>
    def cloudContext = cloud.blobContext
    override def toString() = scheme.schemeName+"://"+container
    def blobStore = cloudContext.createInputStreamMap(container)
    def scheme = CloudFiles
    def /(element : String) = makePath(Array(element))
    def /(path : RelativePath) = makePath(path.elements)
    def /(path : AbsolutePath[CloudFilesUrl]) = makePath(path.elements)
    def makePath(elems : Seq[String]) : CloudFilesUrl = new CloudFilesUrl {
      val urlBase = thisUrlBase
      val elements : Seq[String] = elems.toArray[String]
    }
    override def equals(that : Any) : Boolean = that.isInstanceOf[CloudFilesUrlBase] &&
        container == that.asInstanceOf[CloudFilesUrlBase].container
  }

  implicit object CloudFilesStreamCharWriter extends StreamWriter[CloudFilesUrl, Char] {
    def output(url : CloudFilesUrl, append : Boolean = false) : Output[Char] = {
      val sw = new StringWriter
      new CharOutput(sw) {
        override def flush() = ()
        override def close() = {
          sw.close()
          val ub = url.urlBase.asInstanceOf[CloudFilesUrlBase]
          val bc = ub.cloudContext
          val bs = bc.createInputStreamMap(ub.container)
          bs.putBytes(url.elements.mkString("/"), sw.toString.getBytes("UTF-8")) // FIXME: Encoding
          bc.close()
        }
      }
    }
  }
  
}

class Cloud(username : String, secretKey : String) {

  val cloudService = "cloudservers-uk"

  def blobContext =
    new BlobStoreContextFactory().createContext("cloudfiles-uk", username, secretKey)
  
  def process[T](block : CloudServersClient => T) = {
    val context = new ComputeServiceContextFactory().createContext(cloudService, username, secretKey)
    val rackspaceClient = classOf[CloudServersClient].cast(context.getProviderSpecificContext().getApi())
    val r = block(rackspaceClient)
    context.close()
    r
  }

  def reboot(vmId : Int, hard : Boolean = false) =
    process(_.rebootServer(vmId, if(hard) RebootType.HARD else RebootType.SOFT))

  def provision(vmName : String, imageId : Int, flavor : Int, identifier : String)(implicit log : LogService) : (Int, IpAddress, IpAddress, String) =
    process { ctx =>
      log.info("Got service context")
      val server = ctx.createServer(vmName, imageId, flavor,
          withFile("/etc/sseident", identifier.getBytes("UTF-8")))

      log.info("Created server "+server)
      (server.getId, IpAddress.parse(server.getAddresses.getPublicAddresses.head),
          IpAddress.parse(server.getAddresses.getPrivateAddresses.head),
          server.getAdminPass)
    }

  def decommission(vmId : Int)(implicit log : LogService) : Unit =
    process(_.deleteServer(vmId))

  def images() = process { ctx =>
    ctx.listImages(startAt(0).maxResults(1000))
  }

}
