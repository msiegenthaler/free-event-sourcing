package slfesakka

import akka.actor.{ Actor, ActorRef, Props }
import cats.data.Xor
import slfes.{ AggregateImplementation, Cmd }

/** Manages a single aggregate */
object AkkaAggregate {
  case class ExecuteCommand(id: CommandId, cmd: Cmd)

  def props(aggregate: AggregateImplementation, eventBus: ActorRef)(id: aggregate.Id) =
    Props(new Impl[aggregate.type, aggregate.Id](aggregate, eventBus, id))

  private class Impl[A <: AggregateImplementation { type Id = I }, I](aggregate: A, eventBus: ActorRef, id: I) extends Actor {
    def receive = handle(aggregate.seed(id))
    def handle(state: aggregate.State): Receive = {
      case ExecuteCommand(commandId, cmd: Cmd) ⇒
        aggregate.AnyToCommand(cmd) match {
          case Some(command) ⇒
            aggregate.handleCommand(command)(state) match {
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

          case None ⇒
            sender() ! CommandFailed(commandId, s"Received invalid command type: ${cmd.getClass.getName}")
        }
    }
  }
}