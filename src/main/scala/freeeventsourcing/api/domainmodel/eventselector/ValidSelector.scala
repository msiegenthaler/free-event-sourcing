package freeeventsourcing.api.domainmodel.eventselector

import scala.annotation.implicitNotFound
import freeeventsourcing.api.domainmodel.EventSelector.{ AfterSelector, FilteredSelector, WithEventType }
import freeeventsourcing.api.domainmodel.BoundedContext

@implicitNotFound("${S} is not a valid EventSelector for this bounded context (${BC}).")
trait ValidSelector[BC <: BoundedContext, S]

object ValidSelector {
  implicit def afterOfValid[BC <: BoundedContext, S <: WithEventType: ValidSelector[BC, ?]] =
    new ValidSelector[BC, AfterSelector[S]] {}
  implicit def filteredOfValid[BC <: BoundedContext, S <: WithEventType: ValidSelector[BC, ?], E] =
    new ValidSelector[BC, FilteredSelector[S, E]] {}
}