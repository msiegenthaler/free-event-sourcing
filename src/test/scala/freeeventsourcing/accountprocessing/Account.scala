package freeeventsourcing.accountprocessing

import shapeless.{ :+:, CNil }
import freeeventsourcing.api.{ Aggregate, AggregateCommand, DomainEvent }

object Account extends Aggregate {
  val name = "Account"

  final case class Id(id: Long)

  sealed trait Event extends DomainEvent
  object Event {
    final case class Opened(owner: String) extends Event
    final case class Blocked(by: Transaction.Id, amount: Long) extends Event
    final case class Announced(by: Transaction.Id, amount: Long) extends Event
    final case class BalanceChanged(newBalance: Long, byTx: Transaction.Id, byAmount: Long) extends Event
    final case class TxAborted(tx: Transaction.Id) extends Event
    final case class Closed() extends Event
  }

  sealed trait Command extends AggregateCommand
  object Command {
    import Error._
    final case class Open(owner: String) extends Command {
      type Error = AlreadyOpen :+: CNil
    }
    final case class Close() extends Command {
      type Error = NotOpen :+: NotEmpty :+: HasPendingTx :+: CNil
    }
    final case class BlockFunds(tx: Transaction.Id, amount: Long) extends Command {
      type Error = InsufficientFunds :+: NotOpen :+: CNil
    }
    final case class AnnounceDeposit(tx: Transaction.Id, amount: Long) extends Command {
      type Error = NotOpen :+: CNil
    }
    final case class ConfirmTransaction(tx: Transaction.Id) extends Command {
      type Error = TxNotFound :+: NotOpen :+: CNil
    }
    final case class CancelTransaction(tx: Transaction.Id) extends Command {
      type Error = TxNotFound :+: NotOpen :+: CNil
    }
  }

  object Error {
    final case class AlreadyOpen()
    final case class NotOpen()
    final case class InsufficientFunds()
    final case class TxNotFound(tx: Transaction.Id)
    final case class NotEmpty()
    final case class HasPendingTx(txs: List[Transaction.Id])
  }
}