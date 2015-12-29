package slfes.example

import shapeless._
import slfes._

object Example {
  case class Id(id: Long)

  case class Account(id: Id, owner: Option[String], open: Boolean)

  object Errors {
    case class AlreadyOpen()
    case class NotOpen()
  }

  object Command {
    import Errors._
    sealed trait Base
    case class Open(owner: String) extends Base with Command[AlreadyOpen :+: CNil]
    case class Close(force: Boolean) extends Base with Command[NotOpen :+: CNil]
  }
  object Commands extends CoproductFromBase {
    val generic = Generic[Command.Base]
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

  object Handle extends CommandHandler[Account, Commands.Type, Events.Type] {
    import Command._
    import Errors._
    import Event._

    implicit val open = on[Open] { c ⇒ s ⇒
      if (!s.open) c.success(Opened(c.owner))
      else c.fail(AlreadyOpen())
    }

    implicit val close = onM[Close](c ⇒ for {
      _ ← if (!c.state.open) c.fail(NotOpen()) else c.noop
      _ ← c.emit(Closed())
    } yield ())
  }

  object Apply extends EventApplicator[Account, Events.Type] {
    import Event._
    implicit val opened = on[Opened] { e ⇒ s ⇒
      s.copy(open = true, owner = Some(e.owner))
    }
    implicit val closed = on[Closed] { e ⇒ s ⇒
      s.copy(open = false)
    }
    implicit val balanced = on[Balanced] { e ⇒ identity }
  }


  AggregateType(
    name = "Accout",
    seed = seed,
    handleCommand = handle,
    applyEvent = apply
  )
  private def seed(id: Id) = Account(id, None, false)
  private def handle(cmd: Commands.Type) = cmd.fold(Handle)
  private def apply(event: Events.Type) = event.fold(Apply)
}
