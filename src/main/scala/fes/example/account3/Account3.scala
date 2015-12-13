package fes.example.account3

import scala.language.higherKinds
import scala.language.existentials
import java.util.UUID
import fes.{CommandSyntax, AggregateType, CommandEffect}

case class Id(private val uuid: UUID) extends AnyVal

case class Account(id: Id, owner: Option[String], balance: Int, open: Boolean)
object Account {
  def create(id: Id) = Account(id, None, 0, false)
}

object Commands {
  sealed trait Command[+A]
  sealed trait CommandWithId[+A] extends Command[A] {
    def account: Id
  }
  case class Open(owner: String) extends Command[Unit]
  case class Close(account: Id) extends CommandWithId[Unit]
  case class Debit(account: Id, amount: Int) extends CommandWithId[Unit]
  case class Credit(account: Id, amount: Int) extends CommandWithId[Unit]
}

object Events {
  sealed abstract class Event(val apply: Account ⇒ Account)
  case class Opened(owner: String) extends Event(_.copy(owner = Some(owner), open = true))
  case class Closed() extends Event(_.copy(open = false))
  case class Debited(amount: Int, newBalance: Int) extends Event(account ⇒ account.copy(balance = newBalance))
  case class Credited(amount: Int, newBalance: Int) extends Event(account ⇒ account.copy(balance = newBalance))
}


private object CommandHandler {
  import Commands._, Events._
  val syntax = CommandSyntax[Command, Event, Account]
  import syntax._

  def id(cmd: Command[_]) = cmd match {
    case Open(_) ⇒ Id(UUID.randomUUID())
    case c: CommandWithId[_] ⇒ c.account
  }

  def handle[A](cmd: Command[A]) = cmd match {
    case c@Open(owner) ⇒
      for {
        _ ← emit(Opened(owner))
      } yield ()

    case Close(_) ⇒
      for {
        _ ← when(_.open, emit(Closed()))
      } yield ()

    case Credit(_, amount) ⇒
      for {
        _ ← mustBeOpen
        _ ← emitS(c ⇒ Credited(amount, c.balance + amount))
      } yield ()

    case Debit(_, amount) ⇒
      for {
        _ ← mustBeOpen
        _ ← precondition(_.balance >= amount, s"need at least $amount in the account")
        _ ← emitS(c ⇒ Debited(amount, c.balance - amount))
      } yield ()
  }

  private def mustBeOpen = precondition(_.open, s"accounts need to be open")
}

object AccountAggregate {
  val tpe = AggregateType[Id, Account, Commands.Command, Events.Event](
    "Account",
    Account.create,
    CommandHandler.id _,
    CommandHandler.handle,
    (e: Events.Event) ⇒ e.apply)
}