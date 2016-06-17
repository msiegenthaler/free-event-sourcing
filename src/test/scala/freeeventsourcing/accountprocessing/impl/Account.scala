package freeeventsourcing.accountprocessing.impl

import cats.data.Xor
import scala.collection.immutable.Seq
import freeeventsourcing.accountprocessing.Account.Command._
import freeeventsourcing.accountprocessing.Account.Error._
import freeeventsourcing.accountprocessing.Account.Event._
import freeeventsourcing.accountprocessing.Account._
import freeeventsourcing.accountprocessing.Transaction
import freeeventsourcing.syntax.{ CoproductCommandHandler, CoproductEventApplicator }

case class PendingTx(id: Transaction.Id, amount: Long, isDebit: Boolean)
final case class AccountState(owner: Option[String], open: Boolean, balance: Long, pending: Map[Transaction.Id, PendingTx]) {
  def unblockedBalance = balance - pendingAmount
  def pendingAmount = pending.values.filter(_.isDebit).map(_.amount).sum
}
object AccountState {
  def initial(id: Id) = AccountState(None, false, 0, Map.empty)
}

private object AccountHandler extends CoproductCommandHandler[Command, AccountState, Event] {
  def handle[C <: Command](command: C, state: AccountState) = doHandle(command).apply(state)

  implicit val open = onM[Open](c ⇒ for {
    _ ← c.failIf(c.state.open)(AlreadyOpen())
    _ ← c.emit(Opened(c.cmd.owner))
  } yield ())

  implicit val block = onM[BlockFunds](c ⇒ for {
    _ ← c.assertThat(c.state.open)(NotOpen())
    _ ← c.assertThat(c.state.unblockedBalance > c.cmd.amount)(InsufficientFunds())
    _ ← c.emit(Blocked(c.cmd.tx, c.cmd.amount))
  } yield ())

  implicit val deposit = onM[AnnounceDeposit](c ⇒ for {
    _ ← c.assertThat(c.state.open)(NotOpen())
    _ ← c.emit(Announced(c.cmd.tx, c.cmd.amount))
  } yield ())

  implicit val confirmTx = onM[ConfirmTransaction](c ⇒ for {
    _ ← c.assertThat(c.state.open)(NotOpen())
    txO = c.state.pending.get(c.cmd.tx)
    _ ← c.assertThat(txO.isDefined)(TxNotFound(c.cmd.tx))
    tx = txO.get
    delta = if (tx.isDebit) -tx.amount else tx.amount
    _ ← c.emit(BalanceChanged(c.state.balance + delta, tx.id, tx.amount))
  } yield ())

  implicit val cancelTx = onM[CancelTransaction](c ⇒ for {
    _ ← c.assertThat(c.state.open)(NotOpen())
    _ ← c.assertThat(c.state.pending.contains(c.cmd.tx))(TxNotFound(c.cmd.tx))
    _ ← c.emit(TxAborted(c.cmd.tx))
  } yield ())

  implicit val close = onM[Close](c ⇒ for {
    _ ← c.assertThat(c.state.open)(NotOpen())
    _ ← c.assertThat(c.state.pending.isEmpty)(HasPendingTx(c.state.pending.keys.toList))
    _ ← c.assertThat(c.state.balance == 0)(NotEmpty())
    _ ← c.emit(Closed())
  } yield ())
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