package freeeventsourcing.accountprocessing.impl

import scala.collection.immutable.Seq
import freeeventsourcing.accountprocessing.Account
import freeeventsourcing.accountprocessing.Transaction.Command._
import freeeventsourcing.accountprocessing.Transaction.Error._
import freeeventsourcing.accountprocessing.Transaction.Event._
import freeeventsourcing.accountprocessing.Transaction._
import freeeventsourcing.syntax.{ CoproductCommandHandler, MatchEventApplicator }
import freeeventsourcing.utils.ADT

sealed trait TxState extends ADT
object TxState {
  final case object Unconfirmed extends TxState
  final case object Confirmed extends TxState
  final case object Canceled extends TxState
}
final case class TransactionState(from: Account.Id, to: Account.Id, amount: Long, state: TxState)
object TransactionState {
  def initial(id: Id) = Option.empty[TransactionState]
}

private object TransactionHandler extends CoproductCommandHandler[Command, Event, Option[TransactionState]] {
  implicit val create = onM[Create](c ⇒ for {
    _ ← c.assertThat(c.state.isEmpty)(AlreadyExists())
    _ ← c.emit(Created(c.cmd.from, c.cmd.to, c.cmd.amount))
  } yield ())

  implicit val confirm = onM[Confirm](c ⇒ for {
    _ ← c.assertThat(c.state.isDefined)(DoesNotExist())
    state = c.state.get
    _ ← c.failIf(state.state == TxState.Canceled)(AlreadyCanceled())
    _ ← c.emitIf(state.state == TxState.Unconfirmed)(Confirmed())
  } yield ())

  implicit val cancel = onM[Cancel](c ⇒ for {
    _ ← c.assertThat(c.state.isDefined)(DoesNotExist())
    state = c.state.get
    _ ← c.failIf(state.state == TxState.Confirmed)(AlreadyConfirmed())
    _ ← c.emitIf(state.state == TxState.Unconfirmed)(Canceled())
  } yield ())

  def handle[C <: Command](command: C, state: State) = doHandle(command).apply(state)
}

private object TransactionApplicator extends MatchEventApplicator[Event, Option[TransactionState]] {
  def apply(event: Event, state: Option[TransactionState]) = event match {
    case Created(from, to, amount) ⇒
      val info = TransactionState(from, to, amount, TxState.Unconfirmed)
      Some(info)
    case Confirmed() ⇒ state.map(_.copy(state = TxState.Confirmed))
    case Canceled()  ⇒ state.map(_.copy(state = TxState.Canceled))
  }
}