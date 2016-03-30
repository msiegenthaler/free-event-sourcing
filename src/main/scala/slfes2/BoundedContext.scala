package slfes2

import shapeless.LUBConstraint.<<:
import shapeless.{ HList, HNil, LUBConstraint, :: }

import scala.annotation.implicitNotFound

sealed trait BoundedContext {
  type Aggregates <: HList
  val aggregates: Aggregates
}
object BoundedContext {
  def apply[As <: HList: AggregateConstraint: AggregateImplementations](instances: As) = new BoundedContext {
    type Aggregates = As
    val aggregates = instances
    protected val implementations = AggregateImplementations[As].implementations
  }

  @implicitNotFound("Not all elements of HList are aggregates: ${AS}")
  type AggregateConstraint[AS <: HList] = LUBConstraint[AS, Aggregate]

  @implicitNotFound("Could not find an AggregateImplementation for one of the aggregates in ${As}")
  sealed trait AggregateImplementations[As <: HList] {
    type Impls <: HList
    def implementations: Impls
    def implementationList: List[AggregateImplementation[_]]
  }
  object AggregateImplementations {
    def apply[A <: HList](implicit i: AggregateImplementations[A]) = i
    type Aux[As <: HList, Is <: HList] = AggregateImplementations[As] { type Imples = Is }
    implicit val forHNil = new AggregateImplementations[HNil] {
      type Impls = HNil
      def implementations = HNil
      def implementationList = Nil
    }
    implicit def forHList[H <: Aggregate: AggregateImplementation, T <: HList](implicit ts: AggregateImplementations[T]) =
      new AggregateImplementations[H :: T] {
        type Impls = AggregateImplementation[H] :: ts.Impls
        def implementations = AggregateImplementation[H] :: ts.implementations
        def implementationList = AggregateImplementation[H] :: ts.implementationList
      }
  }
}