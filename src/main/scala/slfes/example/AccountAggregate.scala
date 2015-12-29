package slfes.example

import slfes._
import slfes.utils.AllSingletons
import slfes.example.Account._
import Command._
import Error._
import Event._

private class AccountAggregate {
  sealed trait Invariant extends (State ⇒ Boolean)
  object Invariant {
    object OpenAccountMustHaveOwner extends Invariant {
      def apply(s: State) = if (s.open) s.owner.isDefined else true
    }

    val All: Set[Invariant] = AllSingletons
    def error(failed: Invariant) = Failed(failed.getClass.getSimpleName)
  }

  object Handle extends CommandHandler[State, Commands.Type, Events.Type] {
    import plain._
    import monadic._

    implicit val open = on[Open] { c ⇒ s ⇒
      if (!s.open) c.success(Opened(c.owner))
      else c.fail(AlreadyOpen())
    }

    implicit val close = onM[Close](c ⇒ for {
      _ ← if (!c.state.open) c.fail(NotOpen()) else c.noop
      _ ← c.emit(Closed())
    } yield ())
  }

  object Apply extends EventApplicator[State, Events.Type] {
    implicit val opened = on[Opened] { e ⇒ s ⇒
      s.copy(open = true, owner = Some(e.owner))
    }
    implicit val closed = on[Closed] { e ⇒ s ⇒
      s.copy(open = false)
    }
    implicit val balanced = on[Balanced] { e ⇒ identity }
  }

  def seed(id: Id) = State(id, None, false)

  val description = AggregateWithInvariants.apply[Id, State, Commands.Type, Events.Type, Invariant](
    name = "Account",
    seed = seed,
    handleCommand = _.fold(Handle),
    applyEvent = _.fold(Apply),
    invariants = Invariant.All,
    onInvariantFailed = Invariant.error
  )
}

object AccountAggregate {
  private val impl = new AccountAggregate
  val description = impl.description
}