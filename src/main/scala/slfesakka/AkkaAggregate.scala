package slfesakka

import akka.actor.{ Actor, ActorRef }
import cats.data.Xor
import slfes.AggregateImplementation

class AkkaAggregate[A <: AggregateImplementation](val aggregate: A) {
  case class ExecuteCommand(id: CommandId, cmd: aggregate.Command)
  case class CommandExecuted(id: CommandId)
  case class CommandFailed(id: CommandId, error: Any)

  private class Impl(id: aggregate.Id)(eventBus: ActorRef) extends Actor {
    override def receive = handle(aggregate.seed(id))

    def handle(state: aggregate.State): Receive = {
      case ExecuteCommand(commandId, cmd) ⇒
        aggregate.handleCommand(cmd)(state) match {
          case Xor.Left(err) ⇒
            sender() ! CommandFailed(commandId, err)
            
          case Xor.Right(events) ⇒
            val next = events.foldLeft(state)((s, e) ⇒ aggregate.applyEvent(e)(s))
            events
              .map(e ⇒ EventStoreAdapter.Publish(id, e))
              .foreach(eventBus ! _)
            sender() ! CommandExecuted(commandId)
            context become handle(next)
        }
    }
  }
}
