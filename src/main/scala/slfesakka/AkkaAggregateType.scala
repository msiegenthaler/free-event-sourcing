package slfesakka

import akka.actor.{ Actor, ActorRef, Props }
import slfes.{ AggregateImplementation, Cmd }

/** Manages a class of Aggregates */
object AkkaAggregateType {
  case class ExecuteCommand(id: CommandId, aggregateId: Any, cmd: Cmd)

  def props(aggregate: AggregateImplementation, eventBus: ActorRef) =
    Props(new Impl(aggregate, eventBus))

  class Impl(aggregate: AggregateImplementation, eventBus: ActorRef) extends Actor {
    def receive = handle(Map.empty)
    def handle(instances: Map[aggregate.Id, ActorRef]): Receive = {
      case ExecuteCommand(commandId, aggregateId, cmd) ⇒
        aggregate.IdTypeable.cast(aggregateId) match {
          case Some(id) ⇒
            val actor = instances.getOrElse(id, {
              val a = create(id)
              context become handle(instances + (id → a))
              a
            })
            actor forward AkkaAggregate.ExecuteCommand(commandId, cmd)

          case None ⇒
            sender() ! CommandFailed(commandId, s"Invalid id for aggregate ${aggregate.name}: type was ${aggregateId.getClass.getName}")
        }
    }

    private def create(id: aggregate.Id) = {
      context.actorOf(AkkaAggregate.props(aggregate, eventBus)(id))
    }
  }
}