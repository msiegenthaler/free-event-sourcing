package freeeventsourcing.api

/** Events that are not aggregate event but still relevant/used inside the bounded context. */
trait EventGroup {
  val name: String

  /** Base type of events that are received from this port. */
  type Event <: DomainEvent
}

//TODO add them to bounded context and event selectors..
