package rapture.web

import java.io._
import java.util._
import org.apache.log4j._

import rapture.io._

trait LogService {
  def debug(msg : => String) : Unit
  def info(msg : => String) : Unit
  def warn(msg : => String) : Unit
  def error(msg : => String) : Unit
  def exception(e : => Throwable) : Unit = {
    error(e.toString)
    e.getStackTrace foreach { ste => error("    "+ste) }
  }
}

object NoLogging extends LogService {
  def debug(msg : => String) = ()
  def info(msg : => String) = ()
  def warn(msg : => String) = ()
  def error(msg : => String) = ()
}

trait StandardLogging {
  protected val dateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS ")
  protected def now() = dateFormat.format(new Date)
  protected def writeLog(msg : String, nature : String) : Unit
  def debug(msg : => String) = writeLog(msg, "DEBUG ")
  def info(msg : => String) = writeLog(msg, "INFO  ")
  def warn(msg : => String) = writeLog(msg, "WARN  ")
  def error(msg : => String) = writeLog(msg, "ERROR ")
}

class FileLogger(logFile : FileUrl) extends LogService with StandardLogging {
  private val out = new BufferedWriter(new FileWriter(logFile.javaFile, true))
  protected def writeLog(msg : String, nature : String) = synchronized {
    out.write(now())
    out.write(nature)
    out.write(msg)
    out.write("\n")
    out.flush()
  }
}

object StdoutLogger extends LogService with StandardLogging {
  protected def writeLog(msg : String, nature : String) =
    synchronized { println(dateFormat.format(new Date)+nature+msg) }
}

class Log4jLogAdaptor(logger : org.apache.log4j.Logger) extends LogService {
  def debug(msg : => String) = logger.debug(msg)
  def info(msg : => String) = logger.info(msg)
  def warn(msg : => String) = logger.warn(msg)
  def error(msg : => String) = logger.error(msg)

}
