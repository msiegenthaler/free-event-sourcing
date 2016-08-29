package freeeventsourcing.api.port

import freeeventsourcing.api.EventSelector
import org.reactivestreams.Publisher

/** Port that receives the events that happen inside the bounded context. */
trait PublishedEventsPort {
  //TODO allow to combine multiple selectors.. but basically this is it
  def event[E](selector: EventSelector[E]): Publisher[E]
}
