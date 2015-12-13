package fes.example

import scala.language.higherKinds
import java.util.UUID
import cats.~>
import cats.syntax.flatMap._
import cats.free.Free
import freetocompose.addLiftingFunctions
import freetocompose.Macro
import cats.Monad

object Account {
  type Id = String
  private def newId() = UUID.randomUUID().toString

  object Commands {
    sealed trait Command[+A]
    case class Open(owner: String) extends Command[Id]
    case class Close(id: Id) extends Command[Unit]
    case class Debit(id: Id, amount: Int) extends Command[Unit]
    case class Credit(id: Id, amount: Int) extends Command[Unit]
  }
  val commands = Macro.liftFunctionsVampire[Commands.Command]('Console)

  object Events {
    sealed trait Event[+A] {
      def id: Id
    }
    case class Opened(id: Id, owner: String) extends Event[Id]
    case class Closed(id: Id) extends Event[Unit]
    case class Debited(id: Id, amount: Int) extends Event[Unit]
    case class Credited(id: Id, amount: Int) extends Event[Unit]
  }

  object CommandHandler extends (Commands.Command ~> Events.Event) {
    import Commands._
    import Events._
    def apply[A](fa: Commands.Command[A]): Events.Event[A] = fa match {
      case Open(owner) ⇒
        val id = newId()
        Opened(id, owner)
      //TODO return the id

      case Close(id) ⇒
        Closed(id)

      case Debit(id, amount) ⇒
        Debited(id, amount)
      case Credit(id, amount) ⇒
        //TODO check current balance
        Credited(id, amount)
    }
  }
  //  Free.compile(CommandHandler)
  val cmd = commands.open("Mario")
  val event = cmd.compile(CommandHandler)

  object XX {
    case class State(id: Id, owner: String, balance: Int, open: Boolean)

    object HandlerOps {
      sealed trait Op[+A]
      case class Emit(event: Events.Event[_]) extends Op[Unit]
      case class Fail(reason: String) extends Op[Nothing]
      case class CurrentState() extends Op[State]
    }
    val handler = Macro.liftFunctionsVampire[HandlerOps.Op]('Handler)

    object CommandHandler extends (Commands.Command ~> handler.Handler) {
      import handler._
      import Commands._
      import Events._
      def noop = Monad[Handler].pure(())

      def apply[A](fa: Command[A]): Handler[A] = fa match {
        case Open(owner) ⇒
          val id = newId()
          emit(Opened(id, owner)).map(_ ⇒ id)

        case Close(id) ⇒
          for {
            state ← currentState
            _ ← if (state.open) emit(Closed(id)) else noop
          } yield ()

        case Credit(id, amount) ⇒
          emit(Debited(id, amount)).map(_ ⇒ ())

        case Debit(id, amount) ⇒
          for {
            state ← currentState
            _ ← if (state.balance >= amount) emit(Credited(id, amount))
            else fail(s"Not enough funds (need $amount but only have ${state.balance}")
          } yield ()
      }
    }

    object EventHandler {
      import Events._
      def update(before: Option[State], event: Event[_]): State = event match {
        case Opened(id, owner)   ⇒ State(id, owner, 0, true)
        case Closed(_)           ⇒ before.get.copy(open = false)
        case Debited(_, amount)  ⇒ before.get.copy(balance = before.get.balance - amount)
        case Credited(_, amount) ⇒ before.get.copy(balance = before.get.balance + amount)
      }
    }
    
    
    
  }

  //  object EventHandler extends (Events.Event)

  //commands.open("Mario")

  //  object Command {
  //    // TODO add some validations based on the current state
  //    def open(owner: String) = Free.liftF(Opened(newId(), owner))
  //    def close(id: Id) = Free.liftF(Closed(id))
  //    def debit(id: Id, amount: Int) = Free.liftF(Debited(id, amount))
  //    def credit(id: Id, amount: Int) = Free.liftF(Credited(id, amount))
  //   }
  //
  //  import Event._
  //
  //  object EventHandler extends (Event ~> AggregateOp) {
  //    def apply[A](fa: Event[A]): AggregateOp[A] = fa match {
  //      case Opened(id, owner)   => ???
  //      case Closed(_)           => ???
  //      case Debited(_, amount)  => ???
  //      case Credited(_, amount) => ???
  //    }
  //  }
  //
  //  // TODO not here
  //  sealed trait AggregateOp[A]
}
