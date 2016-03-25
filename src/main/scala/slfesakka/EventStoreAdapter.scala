package slfesakka

import akka.actor.{ ExtendedActorSystem, Extension, ExtensionId, ExtensionIdProvider }
import slfes.utils.StringSerializable

trait EventStoreAdapter extends Extension {
  /** Persistence id for an aggregate. */
  def aggregatePersistenceId[A: StringSerializable](boundedContext: String, aggregateType: String, id: A): String
}

object EventStoreAdapter extends ExtensionId[EventStoreAdapter] with ExtensionIdProvider {
  def lookup = EventStoreAdapter
  def createExtension(system: ExtendedActorSystem) = ??? //TODO
}
