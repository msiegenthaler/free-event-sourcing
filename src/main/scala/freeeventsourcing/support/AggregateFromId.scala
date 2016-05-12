package freeeventsourcing.support

import scala.annotation.implicitNotFound
import shapeless.{ HList, :: }
import freeeventsourcing.Aggregate
import freeeventsourcing.support.AggregateFromId.AggregateWithId

/** Gets the aggregate from the type of the id. */
@implicitNotFound("Aggregate for id type ${Id} not found in ${Aggregates}")
trait AggregateFromId[Id, Aggregates <: HList] {
  type Out <: AggregateWithId[Id]
  def aggregate(aggregates: Aggregates): Out
}
object AggregateFromId {
  def apply[Aggregates <: HList](aggregates: Aggregates) = new FromAggregates(aggregates)

  class FromAggregates[Aggregates <: HList](aggregates: Aggregates) {
    def apply[Id](implicit a: AggregateFromId[Id, Aggregates]): a.Out =
      a.aggregate(aggregates)
    def apply[Id](id: Id)(implicit a: AggregateFromId[Id, Aggregates]): a.Out =
      a.aggregate(aggregates)
  }

  type Aux[I, AS <: HList, A <: AggregateWithId[I]] = AggregateFromId[I, AS] { type Out = A }

  implicit def head[A <: AggregateWithId[Id], Id, T <: HList]: Aux[Id, A :: T, A] = new AggregateFromId[Id, A :: T] {
    type Out = A
    def aggregate(aggregates: A :: T) = aggregates.head
  }
  implicit def tail[A, T <: HList, Id](implicit ev: AggregateFromId[Id, T]): Aux[Id, A :: T, ev.Out] = new AggregateFromId[Id, A :: T] {
    type Out = ev.Out
    def aggregate(aggregates: A :: T) = ev.aggregate(aggregates.tail)
  }

  type AggregateWithId[I] = Aggregate { type Id = I }
}
