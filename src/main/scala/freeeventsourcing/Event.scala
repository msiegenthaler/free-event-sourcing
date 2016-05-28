package freeeventsourcing

import scala.language.implicitConversions
import simulacrum.typeclass

trait EventId

//TODO add a way to compare (before, concurrent, after)
trait EventTime

case class EventMetadata(id: EventId, time: EventTime)

case class EventWithMetadata[E](payload: E, metadata: EventMetadata)
object EventWithMetadata {
  implicit def toEvent[E](ewm: EventWithMetadata[E]): E =
    ewm.payload
}