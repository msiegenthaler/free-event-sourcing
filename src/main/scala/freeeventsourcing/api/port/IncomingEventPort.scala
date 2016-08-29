package freeeventsourcing.api.port

import freeeventsourcing.api.EventGroup
import org.reactivestreams.Publisher
import simulacrum.typeclass

case class IncomingEvent[+Event, Seq](pos: Seq, payload: Event)

/** Port that delivers external events into the bounded context. */
@typeclass trait IncomingEventPort[EG <: EventGroup, Seq] {
  type Event <: EG#Event
  def publisher(lastReceived: Seq): Publisher[IncomingEvent[Event, Seq]]
}
