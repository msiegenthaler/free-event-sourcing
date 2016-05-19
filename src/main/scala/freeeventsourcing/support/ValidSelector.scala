package freeeventsourcing.support

import scala.annotation.implicitNotFound
import freeeventsourcing.{ AggregateEventSelector, BoundedContext }

@implicitNotFound("${S} is not a valid EventSelector for this bounded context (${BC}).")
sealed trait ValidSelector[BC <: BoundedContext, S]
object ValidSelector {
  implicit def aggregate[BC <: BoundedContext, S](implicit ev: AggregateEventSelector.ValidFor[S, BC#Aggregates]) = new ValidSelector[BC, S] {}
}