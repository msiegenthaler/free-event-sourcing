package example.account

import shapeless.{:+:, CNil, Generic}
import slfes.Cmd
import slfes.utils.CoproductFromBase

object Account {
  sealed trait Command extends Cmd
  object Command {
    import Error._
    type InvariantsViolated = Failed :+: NotOpen :+: CNil

    case class Open(owner: String) extends Command {
      type Errors = AlreadyOpen :+: InvariantsViolated
    }
    case class BlockFunds(tx: Txid, amount: Amount) extends Command {
      type Errors = InsufficientFunds :+: InvariantsViolated
    }
    case class AnnounceDeposit(tx: Txid, amount: Amount) extends Command {
      type Errors = InvariantsViolated :+: InvariantsViolated
    }
    case class ConfirmTransaction(tx: Txid) extends Command {
      type Errors = TxNotFound :+: InvariantsViolated
    }
    case class AbortTransaction(tx: Txid) extends Command {
      type Errors = TxNotFound :+: InvariantsViolated
    }
    case class Close() extends Command {
      type Errors = NotOpen :+: NotEmpty :+: HasPendingTx :+: InvariantsViolated
    }
  }
  object Commands extends CoproductFromBase(Generic[Command])

  object Error {
    case class AlreadyOpen()
    case class InsufficientFunds()
    case class TxNotFound(tx: Txid)
    case class Failed(reason: String)
    case class NotOpen()
    case class NotEmpty()
    case class HasPendingTx(txs: Set[Txid])
  }

  sealed trait Event
  object Event {
    case class Opened(owner: String) extends Event
    case class Blocked(by: Txid, amount: Amount, unblockedBalance: Amount) extends Event
    case class Announced(by: Txid, amount: Amount) extends Event
    case class Confirmed(tx: Txid, amount: Amount, isDebit: Boolean, newBalance: Amount) extends Event
    case class Aborted(tx: Txid) extends Event
    case class Closed() extends Event
  }
  object Events extends CoproductFromBase(Generic[Event])

  case class Id(id: Long)
}