package freeeventsourcing.api.port

import freeeventsourcing.api.BoundedContext

//TODO or should we 'invert' that? so that an adapter need to tell what he needs and then the infra
// controls the lifecycle of the adapter
/** The interface into the domain model. */
trait Ports[BC <: BoundedContext] {
  def commandPort: CommandPort
  def incomingEventPort
  // TODO and so on...
}
