package slfes.example

import shapeless._
import slfes._
import slfes.utils.{CoproductFromBase, AllSingletons}

object Account {
  object Command {
    sealed trait Base
    import Error._
    case class Open(owner: String) extends Base with Command[AlreadyOpen :+: CNil]
    case class Close(force: Boolean) extends Base with Command[NotOpen :+: CNil]
  }
  object Commands extends CoproductFromBase {
    val generic = Generic[Command.Base]
  }

  object Error {
    case class AlreadyOpen()
    case class NotOpen()
  }

  object Event {
    sealed trait Base
    case class Opened(owner: String) extends Base
    case class Closed() extends Base
    case class Balanced() extends Base
  }
  object Events extends CoproductFromBase {
    val generic = Generic[Event.Base]
  }

  case class Id(id: Long)
  case class State(id: Id, owner: Option[String], open: Boolean)
}


object AccountImplementation {
  import Account._
  import Command._
  import Error._
  import Event._

  private sealed trait Invariant extends (State ⇒ Boolean)
  private object Invariant {
    object OpenAccountMustHaveOwner extends Invariant {
      def apply(s: State) = if (s.open) s.owner.isDefined else true
    }

    val All: Set[Invariant] = AllSingletons
  }

  private object Handle extends CommandHandler[State, Commands.Type, Events.Type] {
    implicit val open = on[Open] { c ⇒ s ⇒
      if (!s.open) c.success(Opened(c.owner))
      else c.fail(AlreadyOpen())
    }

    implicit val close = onM[Close](c ⇒ for {
      _ ← if (!c.state.open) c.fail(NotOpen()) else c.noop
      _ ← c.emit(Closed())
    } yield ())
  }

  private object Apply extends EventApplicator[State, Events.Type] {
    implicit val opened = on[Opened] { e ⇒ s ⇒
      s.copy(open = true, owner = Some(e.owner))
    }
    implicit val closed = on[Closed] { e ⇒ s ⇒
      s.copy(open = false)
    }
    implicit val balanced = on[Balanced] { e ⇒ identity }
  }

  private def seed(id: Id) = State(id, None, false)

  val aggregateType = AggregateType[Id, State, Commands.Type, Events.Type](
    name = "Accout",
    seed = seed,
    handleCommand = _.fold(Handle),
    applyEvent = _.fold(Apply),
    invariants = Invariant.All
  )
}
