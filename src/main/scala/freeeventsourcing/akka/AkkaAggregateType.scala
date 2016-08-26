package freeeventsourcing.akka

import akka.actor.{ Actor, ActorRef, Props }
import freeeventsourcing.api.{ Aggregate, AggregateCommand }

object AkkaAggregateType {
  case class Execute(id: CommandId, aggregateId: Any, command: AggregateCommand)

  def props[A <: Aggregate: SupportedAggregate](aggregate: A, ider: A#Id ⇒ String) =
    Props(new ActorImpl[A](aggregate, ider))

  private[this] class ActorImpl[A <: Aggregate](aggregate: A, ider: A#Id ⇒ String)(implicit impl: SupportedAggregate[A]) extends Actor {
    var aggregates = Map.empty[A#Id, ActorRef].withDefault(createAggregateActor)

    def createAggregateActor(id: A#Id) = {
      val props = AkkaAggregate.props[A](aggregate, ider)(id)
      context.actorOf(props)
    }

    def receive = {
      case Execute(commandId, Id(aggregateId), command) ⇒
        aggregates.get(aggregateId).foreach( // map has a default, so it will be created
          _ forward AkkaAggregate.Execute(commandId, command)
        )

      case Execute(commandId, invalidId, _) ⇒
        sender() ! CommandInvalid(commandId, s"Invalid id for aggregate ${aggregate.name}: type was ${invalidId.getClass.getName}")
    }

    // Helpers for pattern matching
    private[this] object Id {
      def unapply(a: Any): Option[A#Id] = impl.typeableId.cast(a)
    }
  }
}