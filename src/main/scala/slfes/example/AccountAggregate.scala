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
    object `open Account must have an owner` extends Invariant {
      def apply(s: State) = if (s.open) s.owner.isDefined else true
    }

    val All: Set[Invariant] = AllSingletons
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

  object Handle extends CommandHandlerWithInvariants[State, Commands.Type, Events.Type, Invariant, Failed] {
    import plain._
    import monadic._

    def invariants = Invariant.All
    def invariantError(failed: Invariant) = Failed(InvariantShow.show(failed))
    def applyEvent = _.fold(Apply)

    implicit val open = on[Open] { c ⇒ s ⇒
      if (!s.open) c.success(Opened(c.owner))
      else c.fail(AlreadyOpen())
    }

    implicit val close = onM[Close](c ⇒ for {
      _ ← if (!c.state.open) c.fail(NotOpen()) else c.noop
      _ ← c.emit(Closed())
    } yield ())
  }

  def seed(id: Id) = State(id, None, false)

  val description = AggregateType.apply[Id, State, Commands.Type, Events.Type](
    name = "Account",
    seed = seed,
    handleCommand = _.fold(Handle),
    applyEvent = _.fold(Apply))
}

object AccountAggregate {
  private val impl = new AccountAggregate
  val description = impl.description
}