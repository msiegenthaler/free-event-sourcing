package slfes2

import scala.language.implicitConversions
import scala.annotation.implicitNotFound
import shapeless.{ HList, HMap, Poly2 }
import shapeless.ops.hlist.{ LeftFolder, Selector }
import simulacrum.typeclass

@typeclass trait BoundedContextImplementation[BC <: BoundedContext] {
  val boundedContext: BC

  def forAggregate[A <: Aggregate: MemberAggregate](aggregate: A): AggregateImplementation[A]

  @implicitNotFound("The aggregate ${A} does not exist in this bounded context.")
  type MemberAggregate[A <: Aggregate] = Selector[BC#Aggregates, A]
}

object BoundedContextImplementation {
  def apply[BC <: BoundedContext](bc: BC)(implicit f: AggregateImplementations[bc.Aggregates]): BoundedContextImplementation[BC] = {
    val implMap = bc.aggregates.foldLeft(HMap.empty[BiMapAggregateToImplementation])(FoldToImplMap)
    new BoundedContextImplementation[BC] {
      val boundedContext = bc
      def forAggregate[A <: Aggregate: MemberAggregate](aggregate: A) = {
        implMap.get(aggregate).getOrElse {
          throw new IllegalStateException(s"Implementation for aggregate ${aggregate.name} not found in the bounded context ${bc.name}. " +
            s"This should have been prevented at the type level.")
        }
      }
    }
  }

  @implicitNotFound("Not all aggregates have an implementation. An AggregateImplementation[A] must be in implicit scope for each aggregate out of ${Aggregates}")
  type AggregateImplementations[Aggregates <: HList] = LeftFolder.Aux[Aggregates, HMap[BiMapAggregateToImplementation], FoldToImplMap.type, HMap[BiMapAggregateToImplementation]]

  sealed trait BiMapAggregateToImplementation[A, AI]
  implicit def aggregateToImpl[A <: Aggregate]: BiMapAggregateToImplementation[A, AggregateImplementation[A]] =
    new BiMapAggregateToImplementation[A, AggregateImplementation[A]] {}

  object FoldToImplMap extends Poly2 {
    implicit def aggregateWithImpl[H <: HMap[BiMapAggregateToImplementation], A <: Aggregate: AggregateImplementation] =
      at[H, A] { (acc, a) ⇒
        val implementation = implicitly[AggregateImplementation[A]]
        acc + (a → implementation)
      }
  }
}