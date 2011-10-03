package rapture.web

object fork {
  def apply(block : => Unit) = {
    val t = new Thread { override def run() = block }
    t.start()
    t
  }
}
