package freeeventsourcing.akka

import freeeventsourcing.utils.{ CompositeName, StringSerializable }
import freeeventsourcing.api.{ Aggregate, BoundedContext }

object PersistenceIds {
  def forAggregate[A <: Aggregate](boundedContext: BoundedContext, aggregate: A)(id: A#Id)(implicit ser: StringSerializable[A#Id]): String = {
    val name = CompositeName.root / boundedContext.name / aggregate.name / ser.serializeToString(id)
    name.toString
  }
}