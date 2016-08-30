package freeeventsourcing.akka

import scala.language.implicitConversions
import scala.annotation.implicitNotFound
import shapeless.ops.hlist.LeftFolder
import shapeless.{ HList, HMap, Poly2, Typeable }
import simulacrum.typeclass
import freeeventsourcing.akka.SupportedDomainModel.AggregateMap
import freeeventsourcing.api.domainmodel.{ Aggregate, DomainModel, DomainModelImplementation }
import freeeventsourcing.utils.StringSerializable

@typeclass trait SupportedDomainModel[DM <: DomainModel] extends DomainModelImplementation[DM] {
  def aggregates: AggregateMap
}
object SupportedDomainModel {
  implicit def derive[DM <: DomainModel](implicit
    impl: DomainModelImplementation[DM],
    ef: AggregateExtensions[DM#Aggregates]): SupportedDomainModel[DM] = {
    val aggregates: DM#Aggregates = impl.domainModel.aggregates
    val exts = aggregates.foldLeft(HMap.empty[BiMapAggregateExtensions])(FoldToExtMap)

    new SupportedDomainModel[DM] {
      val domainModel: DM = impl.domainModel
      def forAggregate[A <: Aggregate: MemberAggregate](aggregate: A) = impl.forAggregate(aggregate)
      def aggregateInfo = impl.aggregateInfo
      val aggregates: AggregateMap = {
        impl.aggregateInfo.map { d ⇒
          val ext = exts.get(d.aggregate).getOrElse {
            throw new IllegalStateException(s"Implementation extendsion for aggregate ${d.aggregate.name} not found in the " +
              s"bounded context ${impl.domainModel.name}. This should have been prevented at the type level.")
          }
          val s = SupportedAggregate.derive[d.A](
            d.implementation,
            ext.idSerializable,
            ext.typeableId,
            ext.typeableEvent,
            ext.typeableCommand
          )
          (d.aggregate, s)
        }.toMap
      }
      def processes = impl.processes
    }
  }

  type AggregateMap = Map[Aggregate, SupportedAggregate[_ <: Aggregate]]

  trait AggregateExtension[A <: Aggregate] {
    def aggregate: A
    def idSerializable: StringSerializable[A#Id]
    def typeableId: Typeable[A#Id]
    def typeableEvent: Typeable[A#Event]
    def typeableCommand: Typeable[A#Command]
  }
  sealed trait BiMapAggregateExtensions[A, AE]
  implicit def aggregateToExtension[A <: Aggregate]: BiMapAggregateExtensions[A, AggregateExtension[A]] =
    new BiMapAggregateExtensions[A, AggregateExtension[A]] {}
  object FoldToExtMap extends Poly2 {
    implicit def aggregateWithImpl[H <: HMap[BiMapAggregateExtensions], A <: Aggregate](implicit
      idSer: StringSerializable[A#Id],
      ti: Typeable[A#Id],
      te: Typeable[A#Event],
      tc: Typeable[A#Command]) = {
      at[H, A] { (acc, a) ⇒
        val ext = new AggregateExtension[A] {
          def aggregate = a
          def idSerializable = idSer
          def typeableId = ti
          def typeableEvent = te
          def typeableCommand = tc
        }
        acc + (a → ext)
      }
    }
  }
  @implicitNotFound("Not all aggregates are supported by akka. The implicit scope must contain for each Aggregate (A):\n - a StringSerializer for A#ID")
  type AggregateExtensions[Aggregates <: HList] = LeftFolder.Aux[Aggregates, HMap[BiMapAggregateExtensions], FoldToExtMap.type, HMap[BiMapAggregateExtensions]]
}