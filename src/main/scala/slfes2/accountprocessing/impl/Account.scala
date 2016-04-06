package slfes2.accountprocessing.impl

import cats.data.Xor
import slfes2.accountprocessing.Account.Command._
import slfes2.accountprocessing.Account.Event._
import slfes2.accountprocessing.Account._
import slfes2.syntax.{ CoproductCommandHandler, CoproductEventApplicator }

final case class AccountState(owner: Option[String], open: Boolean)

private object AccountHandler extends CoproductCommandHandler[Command, AccountState, Event] {
  def handle[C <: Command](command: C, state: AccountState) = doHandle(command).apply(state)

  implicit val open = at[Open] { _ ⇒ _: State ⇒
    Xor.right(Seq.empty)
  }

  implicit val close = at[Close] { _ ⇒ _: State ⇒
    Xor.right(Seq.empty)
  }
}

private object AccountApplicator extends CoproductEventApplicator[Event, AccountState] {
  implicit val opened = on[Opened] { evt ⇒ state ⇒
    state.copy(owner = Some(evt.owner), open = true)
  }

  implicit val close = on[Closed] { evt ⇒ state ⇒
    state.copy(open = false)
  }
}