package example.account

import shapeless.Generic
import slfes.AggregateDefinition
import slfes.syntax.{EventApplicator, CommandHandlerWithInvariants}
import slfes.utils.{CoproductFromBase, InvariantShow, AllSingletons}
import Account._
import Command._
import Error._
import Event._

object AccountAggregate {
  /** State */
  case class State(id: Id, owner: Option[String], open: Boolean, pending: Map[Transaction.Id, PendingTx], balance: Amount) {
    def unblockedBalance = balance - pending.values.filter(_.isDebit).map(_.amount).sum
  }
  case class PendingTx(id: Transaction.Id, amount: Amount, isDebit: Boolean)

  val commands = new CoproductFromBase(Generic[Command])
  type Commands = commands.Type
  val events = new CoproductFromBase(Generic[Event])
  type Events = events.Type

  /** Apply the events. */
  private object Apply extends EventApplicator[State, Events] {
    implicit val opened = on[Opened] { e ⇒ s ⇒
      s.copy(open = true, owner = Some(e.owner))
    }
    implicit val blocked = on[Blocked] { e ⇒ s ⇒
      val tx = PendingTx(e.by, e.amount, isDebit = true)
      s.copy(pending = s.pending + (tx.id → tx))
    }
    implicit val announced = on[Announced] { e ⇒ s ⇒
      val tx = PendingTx(e.by, e.amount, isDebit = false)
      s.copy(pending = s.pending + (tx.id → tx))
    }
    implicit val confirmed = on[Confirmed] { e ⇒ s ⇒
      s.copy(pending = s.pending - e.tx, balance = e.newBalance)
    }
    implicit val aborted = on[Aborted] { e ⇒ s ⇒
      s.copy(pending = s.pending - e.tx)
    }
    implicit val closed = on[Closed] { e ⇒ s ⇒
      s.copy(open = false)
    }
  }


  /** Handle the commands. */
  private object Handle extends CommandHandlerWithInvariants[State, Commands, Events, Invariant, InvariantsViolated] {
    import plain._
    import monadic._

    def invariants = Invariant.All
    def invariantError(failed: Invariant) = failed match {
      case Invariant.`Must be open for transactions to happen` ⇒ NotOpen()
      case _ ⇒ Failed(InvariantShow.show(failed))
    }
    def applyEvent = _.fold(Apply)

    implicit val open = on[Open] { c ⇒ s ⇒
      if (!s.open) c.success(Opened(c.owner))
      else c.fail(AlreadyOpen())
    }

    implicit val blockFunds = onM[BlockFunds](c ⇒ for {
      _ ← c.failIf(c.state.unblockedBalance < c.command.amount)(InsufficientFunds())
      _ ← c.emit(Blocked(c.command.tx, c.command.amount, c.state.unblockedBalance - c.command.amount))
    } yield ())

    implicit val announceDeposit = on[AnnounceDeposit] { c ⇒ s ⇒
      c.success(Announced(c.tx, c.amount))
    }

    implicit val confirmTx = onM[ConfirmTransaction](c ⇒ for {
      _ ← c.assertThat(c.state.pending.contains(c.command.tx))(TxNotFound(c.command.tx))
      tx = c.state.pending.get(c.command.tx).get
      newBalance = c.state.balance * (if (tx.isDebit) -1 else 1)
      _ ← c.emit(Confirmed(tx.id, tx.amount, tx.isDebit, newBalance))
    } yield ())

    implicit val abortTx = onM[AbortTransaction](c ⇒ for {
      _ ← c.assertThat(c.state.pending.contains(c.command.tx))(TxNotFound(c.command.tx))
      _ ← c.emit(Aborted(c.command.tx))
    } yield ())

    implicit val close = onM[Close](c ⇒ for {
      _ ← c.assertThat(c.state.open)(NotOpen())
      _ ← c.assertThat(c.state.balance != 0)(NotEmpty())
      _ ← c.assertThat(c.state.pending.isEmpty)(HasPendingTx(c.state.pending.keySet))
      _ ← c.emit(Closed())
    } yield ())
  }


  /** Requirements for valid states. */
  private sealed trait Invariant extends (State ⇒ Boolean)
  private object Invariant {
    case object `Must have an owner if open` extends Invariant {
      def apply(s: State) = s.owner.isDefined || !s.open
    }
    case object `Must be open for transactions to happen` extends Invariant {
      def apply(s: State) = s.open || (s.pending.isEmpty && s.balance == 0)
    }
    case object `Balance must not be negative` extends Invariant {
      def apply(s: State) = s.balance >= 0
    }
    case object `Balance including the blockings must not be negative` extends Invariant {
      def apply(s: State) = s.unblockedBalance >= 0
    }
    case object `Must have a balance of zero if closed` extends Invariant {
      def apply(s: State) = s.open || s.balance == 0
    }
    case object `Must have no outstanding transactions if closed` extends Invariant {
      def apply(s: State) = s.open || s.pending.isEmpty
    }

    val All: Set[Invariant] = AllSingletons
  }

  /** Create a new instance. */
  private def seed(id: Id) = State(id, None, open = false, pending = Map.empty, balance = 0)


  val definition = AggregateDefinition[Id, State, Commands, Events](
    name = "Account",
    seed = seed,
    handleCommand = _.fold(Handle),
    applyEvent = _.fold(Apply)).aggregateType
}