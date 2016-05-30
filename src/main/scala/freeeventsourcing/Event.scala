package freeeventsourcing

import scala.language.implicitConversions
import java.time.Instant

trait EventId

//TODO add a way to compare (before, concurrent, after)
trait EventTime {
  def when: Instant
}

case class EventMetadata(id: EventId, time: EventTime)

case class EventWithMetadata[E](payload: E, metadata: EventMetadata)
object EventWithMetadata {
  implicit def toEvent[E](ewm: EventWithMetadata[E]): E =
    ewm.payload
}