package freeeventsourcing.api

//TODO not too sure about the name.. event is fine, but be might find something better than 'group'
/** Events that are not aggregate event but still relevant/used inside the bounded context. */
trait EventGroup {
  val name: String

  /** Base type of events in this group. */
  type Event <: DomainEvent
}

//TODO add them to bounded context and event selectors..
