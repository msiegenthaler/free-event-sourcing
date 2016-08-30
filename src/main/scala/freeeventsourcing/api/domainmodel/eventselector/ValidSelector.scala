package freeeventsourcing.api.domainmodel.eventselector

import scala.annotation.implicitNotFound
import freeeventsourcing.api.domainmodel.EventSelector.{ AfterSelector, FilteredSelector, WithEventType }
import freeeventsourcing.api.domainmodel.DomainModel

@implicitNotFound("${S} is not a valid EventSelector for this bounded context (${DM}).")
trait ValidSelector[DM <: DomainModel, S]

object ValidSelector {
  implicit def afterOfValid[DM <: DomainModel, S <: WithEventType: ValidSelector[DM, ?]] =
    new ValidSelector[DM, AfterSelector[S]] {}
  implicit def filteredOfValid[DM <: DomainModel, S <: WithEventType: ValidSelector[DM, ?], E] =
    new ValidSelector[DM, FilteredSelector[S, E]] {}
}