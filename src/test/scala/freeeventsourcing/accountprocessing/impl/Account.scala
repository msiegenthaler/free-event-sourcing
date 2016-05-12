package freeeventsourcing.accountprocessing.impl

import cats.data.Xor
import freeeventsourcing.accountprocessing.Account.Command._
import freeeventsourcing.accountprocessing.Account.Event._
import freeeventsourcing.accountprocessing.Account._
import freeeventsourcing.accountprocessing.Transaction
import freeeventsourcing.syntax.{ CoproductCommandHandler, CoproductEventApplicator }

case class PendingTx(id: Transaction.Id, amount: Long, isDebit: Boolean)
final case class AccountState(owner: Option[String], open: Boolean, balance: Long, pending: Map[Transaction.Id, PendingTx])
object AccountState {
  def initial(id: Id) = AccountState(None, false, 0, Map.empty)
}

private object AccountHandler extends CoproductCommandHandler[Command, AccountState, Event] {
  def handle[C <: Command](command: C, state: AccountState) = doHandle(command).apply(state)

  implicit val open = at[Open] { _ ⇒ _: State ⇒
    Xor.right(Seq.empty) //TODO
  }

  implicit val block = at[BlockFunds] { _ ⇒ _: State ⇒
    Xor.right(Seq.empty) //TODO
  }

  implicit val deposit = at[AnnounceDeposit] { _ ⇒ _: State ⇒
    Xor.right(Seq.empty) //TODO
  }

  implicit val confirmTx = at[ConfirmTransaction] { _ ⇒ _: State ⇒
    Xor.right(Seq.empty) //TODO
  }

  implicit val cancelTx = at[CancelTransaction] { _ ⇒ _: State ⇒
    Xor.right(Seq.empty) //TODO
  }

  implicit val close = at[Close] { _ ⇒ _: State ⇒
    Xor.right(Seq.empty) //TODO
  }
}

private object AccountApplicator extends CoproductEventApplicator[Event, AccountState] {
  implicit val opened = on[Opened] { evt ⇒ state ⇒
    state.copy(owner = Some(evt.owner), open = true)
  }

  implicit val blocked = on[Blocked] { evt ⇒ state ⇒
    val tx = PendingTx(evt.by, evt.amount, false)
    state.copy(pending = state.pending + (evt.by → tx))
  }

  implicit val announced = on[Announced] { evt ⇒ state ⇒
    val tx = PendingTx(evt.by, evt.amount, true)
    state.copy(pending = state.pending + (evt.by → tx))
  }

  implicit val balanceChanged = on[BalanceChanged] { evt ⇒ state ⇒
    state.copy(balance = evt.newBalance, pending = state.pending - evt.byTx)
  }

  implicit val txAborted = on[TxAborted] { evt ⇒ state ⇒
    state.copy(pending = state.pending - evt.tx)
  }

  implicit val close = on[Closed] { evt ⇒ state ⇒
    state.copy(open = false)
  }
}