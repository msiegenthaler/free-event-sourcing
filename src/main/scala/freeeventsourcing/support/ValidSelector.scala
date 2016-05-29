package freeeventsourcing.support

import scala.annotation.implicitNotFound
import freeeventsourcing.BoundedContext
import freeeventsourcing.eventselector.AggregateEventSelector

@implicitNotFound("${S} is not a valid EventSelector for this bounded context (${BC}).")
sealed trait ValidSelector[BC <: BoundedContext, S]
object ValidSelector {
  //TODO move over to selector, having it here has the wrong package dependency..
  implicit def aggregate[BC <: BoundedContext, S](implicit ev: AggregateEventSelector.ValidFor[S, BC#Aggregates]) = new ValidSelector[BC, S] {}
  //TODO add for aggregate type
}