package slfes2.accountprocessing.impl

import cats.data.Xor
import slfes2.AggregateImplementation
import slfes2.syntax.{ CoproductCommandHandler, CoproductEventApplicator }
import slfes2.accountprocessing.Account
import Account._
import Command._
import Event._

final case class AccountState(owner: Option[String], open: Boolean)

private object AccountHandler extends CoproductCommandHandler[Command, Event] {
  implicit val open = at[Open] { _ ⇒
    Xor.right(Seq.empty)
  }

  implicit val close = at[Close] { _ ⇒
    Xor.right(Seq.empty)
  }
}

private object AccountApplicator extends CoproductEventApplicator[Event, AccountState] {
  implicit val opened = at[Opened] { evt ⇒ state: AccountState ⇒
    state.copy(owner = Some(evt.owner), open = true)
  }

  implicit val close = at[Closed] { evt ⇒ state: AccountState ⇒
    state.copy(open = false)
  }
}

object AccountImplementation extends AggregateImplementation[Account.type] {
  type State = AccountState
  def seed(id: Id) = AccountState(None, false)
  def applyEvent(event: Event, state: State): State =
    AccountApplicator.apply(event, state)
  def handleCommand[C <: Command](command: C, state: State): C#Error Xor Seq[Event] =
    AccountHandler.handle(command)
}

