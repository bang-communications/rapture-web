package rapture.web

import rapture.io._

// FIXME: This should be reimplemented using invariant types and typeclasses
object Money {
  object Zero extends Money[Nothing](0L, Currencies.XXX)
}

class Money[C <: Money[C]](amt : Double, val currency : Currencies.Currency) { thisMoney =>
  def value = (amt*10000 + 0.5).toLong
  
  def +(m : Money[C]) : Money[C] = new Money[C](0.0, currency) {
    override def value = thisMoney.value + m.value
  }

  def *(x : Int) : Money[C] = new Money[C](0.0, currency) {
    override def value = thisMoney.value*x
  }

  def /(x : Int) : Money[C] = new Money[C](0.0, currency) {
    override def value = thisMoney.value/x
  }

  def -(m : Money[C]) : Money[C] = new Money[C](0.0, currency) {
    override def value = thisMoney.value - m.value
  }

  def unary_- : Money[C] = new Money[C](0.0, currency) {
    override def value = -thisMoney.value
  }
 
  override def toString() = currency.prefix+(value/10000L)+"."+(10000 + value).toString.takeRight(4)
}

case class Eur(v : Double) extends Money[Eur](v, Currencies.EUR)

case class Gbp(v : Double) extends Money[Gbp](v, Currencies.GBP)

case class Usd(v : Double) extends Money[Usd](v, Currencies.USD)

object $ { def apply(v : Double) = Usd(v) }

object Currencies extends Lookup[String] {
  type Item = Currency
  case class Currency(code : String, prefix : String) extends AutoAppend { def index = code }

  val XXX = Currency("XXX", "")

  val AUD = Currency("AUD", "$")
  val CAD = Currency("CAD", "$")
  val CHF = Currency("CHF", "Fr")
  val CNY = Currency("CNY", "¥")
  val DKK = Currency("DKK", "kr")
  val EUR = Currency("EUR", "€")
  val GBP = Currency("GBP", "£")
  val INR = Currency("INR", "Rs")
  val JPY = Currency("JPY", "¥")
  val NOK = Currency("NOK", "kr")
  val NZD = Currency("NZD", "$")
  val RUB = Currency("RUB", "р")
  val USD = Currency("USD", "$")
}
