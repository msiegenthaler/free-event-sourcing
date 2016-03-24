package slfesakka

import akka.actor.{ Actor, ActorRef, Props }
import slfes.{ AggregateImplementation, Cmd }

/** Manages a class of Aggregates */
object AkkaAggregateType {
  case class ExecuteCommand(id: CommandId, aggregateId: Any, cmd: Cmd)

  def props(base: CompositeName, aggregate: AggregateImplementation, eventBus: ActorRef) =
    Props(new Impl(base, aggregate, eventBus))

  class Impl(base: CompositeName, aggregate: AggregateImplementation, eventBus: ActorRef) extends Actor {
    val name = base / aggregate.name
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
            sender() ! CommandInvalid(commandId, s"Invalid id for aggregate ${aggregate.name}: type was ${aggregateId.getClass.getName}")
        }
    }

    private def create(id: aggregate.Id) = {
      context.actorOf(AkkaAggregate.props(name, aggregate, eventBus)(id))
    }
  }
}