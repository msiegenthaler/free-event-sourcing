package example.account

import shapeless.{:+:, CNil}
import slfes.Cmd

/** Banking account. Holds money and keeps track on it, is modified using transactions. */
object Account {
  sealed trait Command extends Cmd
  object Command {
    import Error._
    type InvariantsViolated = Failed :+: NotOpen :+: CNil

    case class Open(owner: String) extends Command {
      type Errors = AlreadyOpen :+: InvariantsViolated
    }
    case class BlockFunds(tx: Transaction.Id, amount: Amount) extends Command {
      type Errors = InsufficientFunds :+: InvariantsViolated
    }
    case class AnnounceDeposit(tx: Transaction.Id, amount: Amount) extends Command {
      type Errors = InvariantsViolated :+: InvariantsViolated
    }
    case class ConfirmTransaction(tx: Transaction.Id) extends Command {
      type Errors = TxNotFound :+: InvariantsViolated
    }
    case class AbortTransaction(tx: Transaction.Id) extends Command {
      type Errors = TxNotFound :+: InvariantsViolated
    }
    case class Close() extends Command {
      type Errors = NotOpen :+: NotEmpty :+: HasPendingTx :+: InvariantsViolated
    }
  }

  object Error {
    case class AlreadyOpen()
    case class InsufficientFunds()
    case class TxNotFound(tx: Transaction.Id)
    case class Failed(reason: String)
    case class NotOpen()
    case class NotEmpty()
    case class HasPendingTx(txs: Set[Transaction.Id])
  }

  sealed trait Event
  object Event {
    case class Opened(owner: String) extends Event
    case class Blocked(by: Transaction.Id, amount: Amount, unblockedBalance: Amount) extends Event
    case class Announced(by: Transaction.Id, amount: Amount) extends Event
    case class Confirmed(tx: Transaction.Id, amount: Amount, isDebit: Boolean, newBalance: Amount) extends Event
    case class Aborted(tx: Transaction.Id) extends Event
    case class Closed() extends Event
  }

  case class Id(id: Long)
}