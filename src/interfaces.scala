package rapture.interfaces

sealed trait Status
object Waiting extends Status
object Initializing extends Status
object Live extends Status
object Stalled extends Status
object Stopping extends Status
object FailedToStart extends Status

trait RaptureService {

  var status : Status = Waiting
  def registerDomainName(domain : String) : Unit
  def loadAvg : List[(Double, Double, Double)]
  def token(m : Int) : String
  def reclaim(token : String, money : Int, tag : Long) : Unit
  def setScaleFunction(fn : List[(Double, Double, Double)] => Int) : Unit
  def projectName : String
  def askQuestion(msg : String) : Unit
  def nodeName : String
  def postNotice(subject : String, message : String) : Unit
}
