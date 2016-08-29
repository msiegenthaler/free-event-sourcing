package freeeventsourcing.api.port

import freeeventsourcing.api.EventSelector
import freeeventsourcing.api.EventSelector.WithEventType
import org.reactivestreams.Publisher

//TODO maybe there's a better name? we must differentate from incoming
//TODO maybe plain 'outgoing' event is better?
/** Port that receives the events that happen inside the bounded context. */
trait PublishedEventsPort {
  //TODO allow to combine multiple selectors.. but basically this is it
  def event[E <: WithEventType](selector: EventSelector[E]): Publisher[E]
}
