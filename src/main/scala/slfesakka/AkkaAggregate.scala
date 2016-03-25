package slfesakka

import akka.actor.{ Actor, ActorRef, Props }
import akka.persistence.PersistentActor
import cats.data.Xor
import shapeless.Coproduct
import slfes.{ AggregateImplementation, Cmd }

/** Manages a single aggregate using a persistent actor.
 *  Only one actor may exist per aggregate (id), else consistence will be violated.
 */
object AkkaAggregate {
  case class ExecuteCommand(id: CommandId, cmd: Cmd)

  def props(boundedContext: String, aggregate: AggregateImplementation)(id: aggregate.Id) =
    Props(new Impl[aggregate.type, aggregate.Id, aggregate.Command, aggregate.Event](boundedContext, aggregate, id))

  private class Impl[A <: AggregateImplementation { type Id = I; type Event = E; type Command = C }, I, C <: Coproduct, E <: Coproduct](
      boundedContext: String, aggregate: A, id: I
  ) extends PersistentActor {
    val persistenceId = EventStoreAdapter(context.system)
      .aggregatePersistenceId(boundedContext, aggregate.name, id)(aggregate.IdStringSerializable)

    private var eventSeq = 0L
    private var state = aggregate.seed(id)
    private def updateState(event: AggregateEvent[E]): Unit = {
      state = aggregate.applyEvent(event.payload)(state)
    }

    //TODO Snapshots would improve performance for aggregates with lots of events

    def receiveRecover = {
      case AggregateEvent(seq, aggregate.Event(evt)) ⇒
        updateState(AggregateEvent(seq, evt))
        eventSeq = seq + 1
      case AggregateEvent(seq, invalidEvt) ⇒
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
              AggregateEvent(seq, e)
            }
            //TODO don't persist the coproduct, extract the value first..
            persistAll[AggregateEvent[E]](toPersist)(updateState _)
        }

      case ExecuteCommand(commandId, invalidCmd) ⇒
        sender() ! CommandInvalid(commandId, s"Received invalid command type: ${invalidCmd.getClass.getName}")
    }
  }
}