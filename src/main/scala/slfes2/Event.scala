package slfes2

import scala.language.implicitConversions
import scala.collection.immutable.Traversable
import simulacrum.typeclass

//case class EventMetadata[A <: Aggregate](from: A#Id, sequence: Long, eventType: EventType, aggregate: A)

trait EventTime {
  //TODO compareable (before, concurrent, after)
}

case class EventMetadata(eventTime: EventTime)
//TODO maybe a "trace"

case class Event[E](
  eventTime: EventTime,
  //    source: EventSource,
  content: E
)

//case class IndexedEvent()

@typeclass trait IndexedEvent[EventKey] {
  def asString(key: EventKey): String
}

trait EventIndexer {
  def apply(e: Event[_]): Traversable[IndexedEvent[_]]
}

object Event {

  type Selector = Event[_] ⇒ List[IndexedEvent[_]]

}