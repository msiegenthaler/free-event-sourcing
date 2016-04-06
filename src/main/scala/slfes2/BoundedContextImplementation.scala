package slfes2

import scala.language.implicitConversions
import scala.annotation.implicitNotFound
import shapeless.ops.hlist.{ LeftFolder, Selector }
import shapeless.{ HList, HMap, Poly2 }
import simulacrum.typeclass
import slfes2.BoundedContextImplementation.BCAggregateInfo

@typeclass trait BoundedContextImplementation[BC <: BoundedContext] {
  val boundedContext: BC
  def forAggregate[A <: Aggregate: MemberAggregate](aggregate: A): AggregateImplementation[A]

  def aggregateInfo: List[BCAggregateInfo[BC]]

  @implicitNotFound("The aggregate ${A} does not exist in this bounded context.")
  type MemberAggregate[A <: Aggregate] = Selector[BC#Aggregates, A]
}

object BoundedContextImplementation {
  def apply[BC <: BoundedContext](bc: BC)(implicit f: AggregateImplementationsEv[bc.Aggregates], l: AggregateInfoEv[BC]): BoundedContextImplementation[BC] = {
    val implMap = bc.aggregates.foldLeft(HMap.empty[BiMapToImpl])(FoldToImplMap)
    new BoundedContextImplementation[BC] {
      val boundedContext = bc
      def forAggregate[A <: Aggregate: MemberAggregate](aggregate: A) = {
        implMap.get(aggregate).getOrElse {
          throw new IllegalStateException(s"Implementation for aggregate ${aggregate.name} not found in the bounded context ${bc.name}. " +
            s"This should have been prevented at the type level.")
        }
      }
      val aggregateInfo = {
        def aggregates: BC#Aggregates = bc.aggregates
        aggregates.foldLeft(List.empty[BCAggregateInfo[BC]])(FoldToAggregateInfo)(l)
      }
    }
  }

  trait BCAggregateInfo[BC <: BoundedContext] {
    type A <: Aggregate
    val aggregate: A
    def implementation: AggregateImplementation[A]
    def memberEvidence: Selector[BC#Aggregates, A]
  }

  @implicitNotFound("Not all aggregates have an implementation. An AggregateImplementation[A] must be in implicit scope for all aggregates in ${Aggregates}")
  type AggregateImplementationsEv[Aggregates <: HList] = LeftFolder.Aux[Aggregates, HMap[BiMapToImpl], FoldToImplMap.type, HMap[BiMapToImpl]]

  @implicitNotFound("Not all aggregates extend the Aggregate trait in bounded context ${BC}.")
  type AggregateInfoEv[BC <: BoundedContext] = LeftFolder.Aux[BC#Aggregates, List[BCAggregateInfo[BC]], FoldToAggregateInfo.type, List[BCAggregateInfo[BC]]]

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
    implicit def aggregate[AG <: Aggregate, BC <: BoundedContext](implicit impl: AggregateImplementation[AG], ev: Selector[BC#Aggregates, AG]) = {
      at[List[BCAggregateInfo[BC]], AG] { (acc, a) ⇒
        val data = new BCAggregateInfo[BC] {
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