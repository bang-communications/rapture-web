package rapture.web

import org.osgi.framework._ // {ServiceReference, BundleContext}

object Osgi {

  sealed abstract class ServiceEvent[Service](service : Service)

  case class Add[Service](service : Service)
      extends ServiceEvent[Service](service)
  case class Change[Service](service : Service)
      extends ServiceEvent[Service](service)
  case class Remove[Service](service : Service)
      extends ServiceEvent[Service](service)

  /** Gets the service object from the OSGi context. */
  // FIXME: Clearly doesn't work!
  def service[Service](implicit ctx : BundleContext, mf : Manifest[Service]) : Option[Service] = {
    if(ctx == null) throw new Exception("ctx == null")
    if(mf == null) throw new Exception("mf == null")
    if(mf.erasure == null) throw new Exception("mf.erasure == null")
    if(mf.erasure.getName == null) throw new Exception("mf.erasure.getName == null")
    if(ctx.getServiceReference(mf.erasure.getName) == null) throw new Exception("etc == null")
    fromNull(ctx.getServiceReference(mf.erasure.getName)) flatMap { sr => try {
      fromNull(ctx.getService(sr)).map(_.asInstanceOf[Service])
    } finally {
      ctx.ungetService(sr)
      None
    } }
  }

  def registerService[Service](svc : Service)(implicit ctx : BundleContext,
      mf : Manifest[Service]) = ctx.registerService(mf.erasure.getName, svc, null)

  /** Tracks service events (addition, removal, modification) on the specified service, and provides
    * a means of performing actions upon these events. */
  def trackService[Service](fn : PartialFunction[ServiceEvent[Service], Unit])
      (implicit ctx : BundleContext, mf : Manifest[Service], log : LogService) = {
    //println("Manifest "+mf.erasure.getName+".")
    val tracker = new org.osgi.util.tracker.ServiceTracker(ctx, ctx.createFilter(
        "(objectclass="+mf.erasure.getName+")"), null) {
      override def addingService(sr : ServiceReference) : Object = {
        log.debug("Adding service "+sr.getBundle.getSymbolicName)
	val svc = service[Service].get
        fn.lift(Add(svc))
	svc.asInstanceOf[Object]
      }

      override def modifiedService(sr : ServiceReference, svc : AnyRef) = {
        log.debug("Modifying service "+sr.getBundle.getSymbolicName)
        fn.lift(Change(svc.asInstanceOf[Service]))
      }

      override def removedService(sr : ServiceReference, svc : AnyRef) = {
        log.debug("Removing service "+sr.getBundle.getSymbolicName)
        fn.lift(Remove(svc.asInstanceOf[Service]))
      }
    }
    tracker.open()
    tracker
  }

  /** Ensures that the value referred to by the setter always maintains a reference to the specified
    * service. */
  // FIME: probably remove
  def maintainService[Service](setter : Option[Service] => Unit)(implicit ctx : BundleContext,
      mf : Manifest[Service], log : LogService) = trackService[Service] {
    case Add(svc) => setter(Some(svc))
    case Remove(_) => setter(None)
  }

  /** Ensures that a collection of services are maintained */
  // FIME: probably remove
  def maintainServices[Service](svcs : List[Service], setter : List[Service] => Unit)
      (implicit ctx : BundleContext, mf : Manifest[Service], log : LogService) =
    trackService[Service] {
      case Add(svc) => svcs.synchronized { setter(svc :: svcs) }
      case Remove(svc) => svcs.synchronized { setter(svcs.filter(_ != svc)) }
    }
}
