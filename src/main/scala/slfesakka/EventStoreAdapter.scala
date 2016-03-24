package slfesakka

import slfes.AggregateImplementation

object EventStoreAdapter {
  case class PublishAggregateEvent[A <: AggregateImplementation](aggregate: A, id: A#Id, event: A#Event)
}
