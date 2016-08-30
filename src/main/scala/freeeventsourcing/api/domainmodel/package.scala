package freeeventsourcing.api

import scala.annotation.implicitNotFound
import freeeventsourcing.utils.ADT
import shapeless.ops.hlist.Selector

package object domainmodel {
  @implicitNotFound("Aggregate ${A} is not a member of this bounded context.")
  type ValidAggregate[BC <: BoundedContext, A <: Aggregate] = Selector[BC#Aggregates, A]

  type DomainEvent = ADT
}
