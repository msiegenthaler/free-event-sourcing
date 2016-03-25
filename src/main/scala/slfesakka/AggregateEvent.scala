package slfesakka

import slfes.EventType

case class AggregateEvent[E](sequence: Long, eventType: EventType, payload: E)

