package slfesakka

import akka.actor.{ Actor, ActorRef, Props }
import akka.persistence.PersistentActor
import cats.Show
import cats.data.Xor
import slfes.{ AggregateImplementation, Cmd }

/** Manages a single aggregate using a persistent actor.
 *  Only one actor may exist per aggregate (id), else consistence will be violated.
 */
object AkkaAggregate {
  case class ExecuteCommand(id: CommandId, cmd: Cmd)

  private case class Event[E](sequence: Long, payload: E)

  def props(base: CompositeName, aggregate: AggregateImplementation, eventBus: ActorRef)(id: aggregate.Id) =
    Props(new Impl[aggregate.type, aggregate.Id, aggregate.Command, aggregate.Event](base, aggregate, eventBus, id))

  private class Impl[A <: AggregateImplementation { type Id = I; type Event = E; type Command = C }, I, C, E](base: CompositeName, aggregate: A, eventBus: ActorRef, id: I)
      extends PersistentActor {
    val name = base / aggregate.Id.serialize(id)
    def persistenceId = name.toString

    private var eventSeq = 0L
    private var state = aggregate.seed(id)
    private def updateState(event: Event[E]): Unit = {
      state = aggregate.applyEvent(event.payload)(state)
    }

    //TODO Snapshots would improve performance for aggregates with lots of events

    def receiveRecover = {
      case Event(seq, aggregate.Event(evt)) ⇒
        updateState(Event(seq, evt))
        eventSeq = seq + 1
      case Event(seq, invalidEvt) ⇒
        throw new IllegalStateException(s"Cannot apply event for ${aggregate.name} (${id}): " +
          s"Unsupported type ${invalidEvt.getClass.getName} @ ${seq}")
    }

    def receiveCommand = {
      case ExecuteCommand(commandId, aggregate.Command(cmd)) ⇒
        aggregate.handleCommand(cmd)(state) match {
          case Xor.Left(err) ⇒
            sender() ! CommandFailed(commandId, err)

          case Xor.Right(events) ⇒
            val toPersist = events.map { e ⇒
              val seq = eventSeq
              eventSeq = eventSeq + 1
              Event[E](seq, e)
            }
            persistAll[Event[E]](toPersist)(updateState _)
        }

      case ExecuteCommand(commandId, invalidCmd) ⇒
        sender() ! CommandInvalid(commandId, s"Received invalid command type: ${invalidCmd.getClass.getName}")
    }
  }
}