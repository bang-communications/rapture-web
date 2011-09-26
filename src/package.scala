package rapture

package object web {

  import java.io._

  def fromNull[T <: AnyRef](t : T) : Option[T] = if(t == null) None else Some(t)

  /** Convenience method on Readers */
  implicit def richReader(in : Reader) = new {
    def pumpTo(out : Writer, flush : Boolean = false) : Int = {
      val buf = new Array[Char](65536)
      var len = in.read(buf)
      var count = 0
      while(len > -1) {
        out.write(buf, 0, len)
        if(flush) out.flush()
        count += len
        len = in.read(buf)
      }
      count
    }
  }
  /** Convenience method on InputStreams */
  implicit def richInputStream(in : InputStream) = new {
    def pumpTo(out : OutputStream, flush : Boolean = false) : Int = {
      val buf = new Array[Byte](65536)
      var len = in.read(buf)
      var count = 0
      while(len > -1) {
        out.write(buf, 0, len)
        if(flush) out.flush()
        count += + len
        len = in.read(buf)
      }
      count
    }
  }

}
