package freeeventsourcing

trait EventId

//TODO add a way to compare (before, concurrent, after)
trait EventTime

case class EventMetadata(id: EventId, time: EventTime)

case class EventWithMetadata[E](event: E, metadata: EventMetadata)
