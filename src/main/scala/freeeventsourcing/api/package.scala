package freeeventsourcing

import scala.annotation.implicitNotFound
import shapeless.ops.hlist.Selector

package object api {
  @implicitNotFound("Aggregate ${A} is not a member of this bounded context.")
  type ValidAggregate[BC <: BoundedContext, A <: Aggregate] = Selector[BC#Aggregates, A]
}