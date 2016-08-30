package freeeventsourcing.akka

import freeeventsourcing.utils.{ CompositeName, StringSerializable }
import freeeventsourcing.api.domainmodel.{ Aggregate, DomainModel }

object PersistenceIds {
  def forAggregate[A <: Aggregate](domainModel: DomainModel, aggregate: A)(id: A#Id)(implicit ser: StringSerializable[A#Id]): String = {
    val name = CompositeName.root / domainModel.name / aggregate.name / ser.serializeToString(id)
    name.toString
  }
}