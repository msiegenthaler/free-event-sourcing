package slfes2.accountprocessing.impl

import cats.data.Xor
import slfes2.syntax.{ CoproductCommandHandler, MatchEventApplicator }
import slfes2.accountprocessing.{ Account, Transaction }
import Transaction._
import Command._
import Event._

final case class TransactionInformation(from: Account.Id, to: Account.Id, amount: Long)
final case class TransactionState(information: Option[TransactionInformation], commited: Boolean)

private object TransactionHandler extends CoproductCommandHandler[Command, TransactionState, Event] {
  def handle[C <: Command](command: C, state: State) = doHandle(command).apply(state)

  implicit val open = at[Create] { _ ⇒ _: State ⇒
    Xor.right(Seq.empty)
  }

  implicit val close = at[Commit] { _ ⇒ _: State ⇒
    Xor.right(Seq.empty)
  }
}

private object TransactionApplicator extends MatchEventApplicator[Event, TransactionState] {
  def apply(event: Event, state: TransactionState) = event match {
    case Created(from, to, amount) ⇒
      val info = TransactionInformation(from, to, amount)
      state.copy(information = Some(info))
    case Commited() ⇒
      state.copy(commited = true)
  }
}
