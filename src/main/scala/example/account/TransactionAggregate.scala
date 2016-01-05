package example.account

import shapeless.{CNil, :+:, Generic}
import slfes.AggregateDefinition
import slfes.syntax.{CommandHandlerWithInvariants, EventApplicator}
import slfes.utils.{InvariantShow, AllSingletons, CoproductFromBase}
import example.account.Transaction._
import Command._
import Event._
import Error._

object TransactionAggregate {
  /** State */
  case class Tx(from: Account.Id, to: Account.Id, amount: Amount, state: TxState)
  sealed trait TxState
  object TxState {
    case object Open extends TxState
    case object Confirmed extends TxState
    case object Canceled extends TxState
  }
  type State = Option[Tx]
  type Commands = Create :+: Confirm :+: Cancel :+: CNil
  type Events = Created :+: Confirmed :+: Canceled :+: CNil


  /** Apply the events. */
  private object Apply extends EventApplicator[State, Events] {
    implicit val created = on[Created] { c ⇒ _ ⇒
      Some(Tx(c.from, c.to, c.amount, TxState.Open))
    }
    implicit val confirmed = on[Confirmed] { c ⇒ s ⇒
      s.map(_.copy(state = TxState.Confirmed))
    }
    implicit val canceled = on[Canceled] { c ⇒ s ⇒
      s.map(_.copy(state = TxState.Confirmed))
    }
  }

  /** Handle the commands. */
  private object Handle extends CommandHandlerWithInvariants[State, Commands, Events, Invariant, InvariantsViolated] {
    import plain._
    import monadic._

    def invariants = Invariant.All
    def invariantError(failed: Invariant) = Failed(InvariantShow.show(failed))
    def applyEvent = _.fold(Apply)

    implicit val create = onM[Create](c ⇒ for {
      cmd ← c.cmdM
      _ ← c.assertThat(cmd.amount > 0)(InvalidAmount())
      _ ← c.emit(Created(cmd.from, cmd.to, cmd.amount))
    } yield ())

    implicit val confirm = onM[Confirm](c ⇒ for {
      _ ← c.assertThat(c.state.isDefined)(Failed("Not created"))
      s = c.state.get
      _ ← c.failIf(s.state == TxState.Canceled)(AlreadyCanceled())
      _ ← c.emitIf(s.state != TxState.Confirmed)(Confirmed(s.from, s.to, s.amount))
    } yield ())

    implicit val cancel = onM[Cancel](c ⇒ for {
      _ ← c.assertThat(c.state.isDefined)(Failed("Not created"))
      s = c.state.get
      _ ← c.failIf(s.state == TxState.Confirmed)(AlreadyConfirmed())
      _ ← c.emitIf(s.state != TxState.Canceled)(Canceled(s.from, s.to))
    } yield ())
  }

  /** Requirements for valid states. */
  private sealed trait Invariant extends (State ⇒ Boolean)
  private object Invariant {
    case object `Must not be empty after creation` extends Invariant {
      def apply(s: State) = s.isDefined
    }
    case object `Amount must be positive` extends Invariant {
      def apply(s: State) = s.map(_.amount >= 0).getOrElse(true)
    }

    val All: Set[Invariant] = AllSingletons
  }

  /** Create a new instance. */
  private def seed(id: Id) = None

  val definition = AggregateDefinition[Id, State, Commands, Events](
    name = "Transaction",
    seed = seed,
    handleCommand = _.fold(Handle),
    applyEvent = _.fold(Apply))
}