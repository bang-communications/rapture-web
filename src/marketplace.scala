package rapture.web

class Coin[C <: Money[C]](val token : String)

trait Marketplace { cloudlet : Cloudlet =>

  def spend[C <: Money[C]](money : C) : Coin[C] = new Coin(raptureService.token(money.value.toInt))

  def reclaim[C <: Money[C]](token : String, money : Money[C], tag : Long) : Unit =
    raptureService.reclaim(token, money.value.toInt, tag)
 
}
