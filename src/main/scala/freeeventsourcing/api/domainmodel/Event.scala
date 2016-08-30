package freeeventsourcing.api.domainmodel

import scala.language.implicitConversions
import java.time.Instant

trait EventId

/** Logical/causual time. Is used to compare the event times.
 *  Since we are dealing with concurrent, distributed systems a lot of events will be 'undecidable', since
 *  they occur at different places and there might not be a global time. What is guaranteed:
 *  - all events e1,e2 that occured on the same aggregate have a stable and definied order (e2 after e1, e1 before e2)
 *  - for an event e1 that triggers another event e2 (via a process): e1 before e2, e2 after e1.
 */
trait EventTime {
  def when: Instant
  def before(other: EventTime): Boolean
  def after(other: EventTime): Boolean
  def earlier(other: EventTime): EventTime
  def later(other: EventTime): EventTime
}
object EventTime {
  /** Earliest time possible */
  object Zero extends EventTime {
    val when = Instant.ofEpochMilli(Long.MinValue)
    def before(other: EventTime) = other != Zero
    def after(other: EventTime) = false
    def earlier(other: EventTime) = this
    def later(other: EventTime) = other
    override def toString = "zero"
  }
}

case class EventMetadata(id: EventId, time: EventTime)

case class EventWithMetadata[+E](payload: E, metadata: EventMetadata)
object EventWithMetadata {
  implicit def toEvent[E](ewm: EventWithMetadata[E]): E =
    ewm.payload
}