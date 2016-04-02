package slfes2.accountprocessing.impl

import cats.data.Xor
import slfes2.syntax.{ CoproductCommandHandler, CoproductEventApplicator }
import slfes2.accountprocessing.Transaction
import Transaction._
import Command._
import Event._

final case class TransactionState()

private object TransactionHandler extends CoproductCommandHandler[Command, TransactionState, Event] {
  def handle[C <: Command](command: C, state: State) = doHandle(command).apply(state)

  implicit val open = at[Create] { _ ⇒ _: State ⇒
    Xor.right(Seq.empty)
  }

  implicit val close = at[Commit] { _ ⇒ _: State ⇒
    Xor.right(Seq.empty)
  }
}

private object TransactionApplicator extends CoproductEventApplicator[Event, TransactionState] {
  implicit val opened = at[Created] { evt ⇒ state: TransactionState ⇒
    state.copy()
  }
  implicit val close = at[Commited] { evt ⇒ state: TransactionState ⇒
    state.copy()
  }
}