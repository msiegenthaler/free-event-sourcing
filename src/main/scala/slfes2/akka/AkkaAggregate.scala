package slfes2.akka

import akka.actor.Props
import akka.persistence.PersistentActor
import cats.data.Xor
import slfes2.{ Aggregate, AggregateCommand }

/** Manages a single aggregate using a persistent actor.
 *  Only one actor may exist per aggregate (id), else inconsistencies will be created.
 */
object AkkaAggregate {
  case class Execute(id: CommandId, command: AggregateCommand)

  def props[A <: Aggregate: SupportedAggregate](aggregate: A, ider: A#Id ⇒ String)(id: A#Id) =
    Props(new ActorImpl[A](aggregate, implicitly, ider)(id))

  private class ActorImpl[A <: Aggregate](aggregate: A, impl: SupportedAggregate[A], ider: A#Id ⇒ String)(id: A#Id) extends PersistentActor {
    val persistenceId = ider(id)

    private var state = impl.seed(id)

    def applyEvent(event: A#Event): Unit = {
      state = impl.applyEvent(event, state)
    }

    def receiveCommand = {
      case Execute(id, Command(command)) ⇒
        impl.handleCommand(command, state) match {
          case Xor.Right(events) ⇒
            persistAll(events)(applyEvent _)
            sender() ! CommandExecuted(id)

          case Xor.Left(error) ⇒
            sender() ! CommandFailed(id, error)
        }

      case Execute(id, notACommand) ⇒
        sender() ! CommandInvalid(id, s"Received invalid command type: ${notACommand.getClass.getName}")
    }

    def receiveRecover = {
      case Event(event) ⇒ applyEvent(event)
    }

    // Helpers for pattern matching
    private object Event {
      def unapply(a: Any): Option[A#Event] = impl.typeableEvent.cast(a)
    }
    private object Command {
      def unapply(a: Any): Option[A#Command] = impl.typeableCommand.cast(a)
    }
  }
}