package freeeventsourcing.api.domainmodel

import scala.language.implicitConversions
import scala.annotation.implicitNotFound
import freeeventsourcing.api.domainmodel.DomainModelImplementation.DMAggregateInfo
import shapeless.ops.hlist.{ LeftFolder, Selector }
import shapeless.{ HList, HMap, Poly2 }
import simulacrum.typeclass

@typeclass trait DomainModelImplementation[DM <: DomainModel] extends AnyRef {
  val domainModel: DM
  def forAggregate[A <: Aggregate: MemberAggregate](aggregate: A): AggregateImplementation[A]

  def aggregateInfo: List[DMAggregateInfo[DM]]

  def processes: List[ProcessDefinition[DM]]

  @implicitNotFound("The aggregate ${A} does not exist in this bounded context.")
  type MemberAggregate[A <: Aggregate] = Selector[DM#Aggregates, A]
}

object DomainModelImplementation {
  def apply[DM <: DomainModel](dm: DM, processes: List[ProcessDefinition[DM]])(
    implicit
    f: AggregateImplementationsEv[dm.Aggregates], l: AggregateInfoEv[DM]
  ): DomainModelImplementation[DM] = {
    val implMap = dm.aggregates.foldLeft(HMap.empty[BiMapToImpl])(FoldToImplMap)
    def p = processes
    new DomainModelImplementation[DM] {
      val domainModel = dm
      def forAggregate[A <: Aggregate: MemberAggregate](aggregate: A) = {
        implMap.get(aggregate).getOrElse {
          throw new IllegalStateException(s"Implementation for aggregate ${aggregate.name} not found in the bounded context ${domainModel.name}. " +
            s"This should have been prevented at the type level.")
        }
      }
      val processes = p
      val aggregateInfo = {
        def aggregates: DM#Aggregates = domainModel.aggregates
        aggregates.foldLeft(List.empty[DMAggregateInfo[DM]])(FoldToAggregateInfo)(l)
      }
    }
  }

  trait DMAggregateInfo[DM <: DomainModel] {
    type A <: Aggregate
    val aggregate: A
    def implementation: AggregateImplementation[A]
    def memberEvidence: Selector[DM#Aggregates, A]
  }

  @implicitNotFound("Not all aggregates have an implementation. An AggregateImplementation[A] must be in implicit scope for all aggregates in ${Aggregates}")
  type AggregateImplementationsEv[Aggregates <: HList] = LeftFolder.Aux[Aggregates, HMap[BiMapToImpl], FoldToImplMap.type, HMap[BiMapToImpl]]

  @implicitNotFound("Not all aggregates extend the Aggregate trait in bounded context ${DM}.")
  type AggregateInfoEv[DM <: DomainModel] = LeftFolder.Aux[DM#Aggregates, List[DMAggregateInfo[DM]], FoldToAggregateInfo.type, List[DMAggregateInfo[DM]]]

  sealed trait BiMapToImpl[A, AI]
  implicit def aggregateToImpl[A <: Aggregate]: BiMapToImpl[A, AggregateImplementation[A]] =
    new BiMapToImpl[A, AggregateImplementation[A]] {}

  object FoldToImplMap extends Poly2 {
    implicit def aggregateWithImpl[H <: HMap[BiMapToImpl], A <: Aggregate: AggregateImplementation] =
      at[H, A] { (acc, a) ⇒
        val implementation = implicitly[AggregateImplementation[A]]
        acc + (a → implementation)
      }
  }

  object FoldToAggregateInfo extends Poly2 {
    implicit def aggregate[AG <: Aggregate, DM <: DomainModel](implicit impl: AggregateImplementation[AG], ev: Selector[DM#Aggregates, AG]) = {
      at[List[DMAggregateInfo[DM]], AG] { (acc, a) ⇒
        val data = new DMAggregateInfo[DM] {
          type A = AG
          val aggregate = a
          def implementation = impl
          def memberEvidence = ev
        }
        data :: acc
      }
    }
  }
}