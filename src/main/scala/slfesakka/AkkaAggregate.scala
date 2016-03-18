package slfesakka

import akka.actor.{ Actor, ActorRef, Props }
import cats.data.Xor
import slfes.AggregateImplementation

/** Manages a single aggregate */
class AkkaAggregate(val aggregate: AggregateImplementation) {
  case class ExecuteCommand(id: CommandId, cmd: aggregate.Command)
  case class CommandExecuted(id: CommandId)
  case class CommandFailed(id: CommandId, error: Any)

  def props(id: aggregate.Id, eventBus: ActorRef) = Props(new Impl(id, eventBus))

  private class Impl(id: aggregate.Id, eventBus: ActorRef) extends Actor {
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
