package slfesakka

case class AggregateEvent[E](sequence: Long, payload: E)

