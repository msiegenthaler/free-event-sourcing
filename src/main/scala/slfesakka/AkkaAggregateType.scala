package slfesakka

import akka.actor.{ Actor, ActorRef, Props }
import slfes.AggregateImplementation

/** Manages a class of Aggregates */
class AkkaAggregateType(aggregateImplementation: AggregateImplementation) {
  val inner = new AkkaAggregate(aggregateImplementation)
  type Id = inner.aggregate.Id
  type Command = inner.aggregate.Command

  case class ExecuteCommand(id: CommandId, target: Id, cmd: Command)
  case class CommandExecuted(id: CommandId)
  case class CommandFailed(id: CommandId, error: Any)

  def props(eventBus: ActorRef) = Props(new Impl(eventBus))

  private class Impl(eventBus: ActorRef) extends Actor {
    override def receive = handle(Map.empty)
    def handle(instances: Map[inner.aggregate.Id, ActorRef]): Receive = {
      case ExecuteCommand(commandId, id, cmd) ⇒
        val actor = instances.getOrElse(id, {
          val a = create(id)
          context become handle(instances + (id → a))
          a
        })
        actor ! inner.ExecuteCommand(commandId, cmd)
    }

    private def create(id: Id) = {
      context.actorOf(inner.props(id, eventBus))
    }
  }
}
