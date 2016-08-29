package freeeventsourcing.api.port

import freeeventsourcing.api.BoundedContext

//TODO or should we 'invert' that? so that an adapter need to tell what he needs and then the infra
// controls the lifecycle of the adapter
//TODO or this might be the result of the bootstraping of the bounded context (or domain model).. then it
// problably should get a different name and be in a different package but in essence it would be this
//TODO I think the second one is the better option because we won't need to centrally manage the lifecycles
// of the adapters then. So it fits better with the "outer controls inner" approach of ports-and-adapters.
/** The interface into the domain model. */
trait Ports[BC <: BoundedContext] {
  def commandPort: CommandPort
  def incomingEventPort: IncomingEventPort[_]
  // TODO and so on...
  //TODO the readmodelport should not be on this.. another indicator that that port is special and should not be
  // treated the same as all the others.. it's more like the event store/event handling infra..
}

trait XX {
  trait Adapter {
    //TODO this is not so nice.. now we have all kinds of problems relating to lifecycle management
    def shutdown: Unit
  }

  def registerAdapter(factory: Ports[_] => Adapter): Unit
}