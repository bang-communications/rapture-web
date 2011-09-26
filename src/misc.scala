package rapture.web

trait Converter[-From, +To] { def apply(x : From) : To }

trait Convertible[This] { this : This =>
  def as[To](implicit converter : Converter[This, To]) = converter(this)
}
