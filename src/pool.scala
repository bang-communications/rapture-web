package rapture.web

import scala.collection.mutable.Queue

/** Implements a dynamic pool of some resource, e.g. database connections. */
abstract class Pool[Resource]
{
  /** Implement to make new resources. */
  protected def make() : Resource

  /** Implement to dispose of surplus resources. */
  protected def dispose(x : Resource) : Unit

  /** Implement to check resource is still usable. */
  protected def check(x : Resource) : Boolean

  /** Number of resource to always keep in reserve, if we have them. */
  protected def spare = 10

  /** How long to leave surplus resources unused before discarding them. */
  protected def timeout = 10*60*1000L

  private val pool = new Queue[Resource]
  private var poolCount = 0
  private var lastLow = 0L

  /** Acquire a resource for the duration of the body. */
  def acquireFor[A](body : Resource => A) : A = {
    val res = acquireDirect()
    try { body(res) } finally { releaseDirect(res) }
  }

  /** Acquire a resource without any nesting guarantees. Avoid this method. */
  def acquireDirect() : Resource = pool.synchronized {
    if(poolCount == 0) make()
    else {
      val r = pool.dequeue
      poolCount = poolCount - 1
      if(check(r)) r
      else {
        dispose(r)
        make()
      }
    }
  }

  /** Release a directly-acquired resource. */
  def releaseDirect(r : Resource) : Unit = pool.synchronized {
    val now = System.currentTimeMillis()
    if(poolCount < spare) lastLow = now
    if(lastLow > now - timeout) {
      pool.enqueue(r)
      poolCount = poolCount + 1
    } else dispose(r)
  }

  /** Dispose of all resources not currently in use. */
  def disposeAll() = pool.synchronized {
    while(poolCount > 0) {
      dispose(pool.dequeue)
      poolCount = poolCount - 1
    }
  }
}
