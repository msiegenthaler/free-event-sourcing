package freeeventsourcing.accountprocessing.impl

import cats.data.Xor
import scala.collection.immutable.Seq
import freeeventsourcing.accountprocessing.Account
import freeeventsourcing.accountprocessing.Transaction.Command._
import freeeventsourcing.accountprocessing.Transaction.Event._
import freeeventsourcing.accountprocessing.Transaction._
import freeeventsourcing.syntax.{ CoproductCommandHandler, MatchEventApplicator }

sealed trait TxState
object TxState {
  case object Unconfirmed extends TxState
  case object Confirmed extends TxState
  case object Canceled extends TxState
}
final case class TransactionState(from: Account.Id, to: Account.Id, amount: Long, state: TxState)
object TransactionState {
  def initial(id: Id) = Option.empty[TransactionState]
}

private object TransactionHandler extends CoproductCommandHandler[Command, Option[TransactionState], Event] {
  def handle[C <: Command](command: C, state: State) = doHandle(command).apply(state)

  implicit val create = at[Create] { _ ⇒ _: State ⇒
    Xor.right(Seq.empty) //TODO
  }

  implicit val confirm = at[Confirm] { _ ⇒ _: State ⇒
    Xor.right(Seq.empty) //TODO
  }

  implicit val cancel = at[Cancel] { _ ⇒ _: State ⇒
    Xor.right(Seq.empty) //TODO
  }
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