package slfes2.accountprocessing.impl

import cats.data.Xor
import slfes2.AggregateImplementation
import slfes2.syntax.{ CoproductCommandHandler, CoproductEventApplicator }
import slfes2.accountprocessing.Transaction
import Transaction._
import Command._
import Event._

final case class TransactionState()

private object TransactionHandler extends CoproductCommandHandler[Command, Event] {
  implicit val open = at[Create] { _ ⇒
    Xor.right(Seq.empty)
  }

  implicit val close = at[Commit] { _ ⇒
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

object TransactionImplementation extends AggregateImplementation[Transaction.type] {
  type State = TransactionState
  def seed(id: Id) = TransactionState()
  def applyEvent(event: Event, state: State): State =
    TransactionApplicator.apply(event, state)
  def handleCommand[C <: Command](command: C, state: State): C#Error Xor Seq[Event] =
    TransactionHandler.handle(command)
}

