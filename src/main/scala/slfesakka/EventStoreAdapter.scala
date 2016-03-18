package slfesakka

object EventStoreAdapter {
  case class Publish[Id, Event](aggregateId: Id, event: Event)
}
