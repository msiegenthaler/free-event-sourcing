package example.account

import shapeless.{ :+:, CNil }
import slfes.Cmd

/** Banking account. Holds money and keeps track on it, is modified using transactions. */
object Account {
  object Command {
    import Error._
    type InvariantsViolated = Failed :+: NotOpen :+: CNil

    case class Open(owner: String) extends Cmd {
      type Errors = AlreadyOpen :+: InvariantsViolated
    }
    case class BlockFunds(tx: Transaction.Id, amount: Amount) extends Cmd {
      type Errors = InsufficientFunds :+: InvariantsViolated
    }
    case class AnnounceDeposit(tx: Transaction.Id, amount: Amount) extends Cmd {
      type Errors = InvariantsViolated :+: InvariantsViolated
    }
    case class ConfirmTransaction(tx: Transaction.Id) extends Cmd {
      type Errors = TxNotFound :+: InvariantsViolated
    }
    case class AbortTransaction(tx: Transaction.Id) extends Cmd {
      type Errors = TxNotFound :+: InvariantsViolated
    }
    case class Close() extends Cmd {
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

  object Event {
    case class Opened(owner: String)
    case class Blocked(by: Transaction.Id, amount: Amount, unblockedBalance: Amount)
    case class Announced(by: Transaction.Id, amount: Amount)
    case class Confirmed(tx: Transaction.Id, amount: Amount, isDebit: Boolean, newBalance: Amount)
    case class Aborted(tx: Transaction.Id)
    case class Closed()
  }

  case class Id(id: Long)
}