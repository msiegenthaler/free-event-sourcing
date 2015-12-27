package slfes

import shapeless._


object Example {
  case class Id(id: Long)

  case class Account(id: Id, owner: Option[String], open: Boolean)

  object Error {
    case class AlreadyOpen()
    case class NotOpen()
  }

  object Command {
    import Error._
    case class Open(owner: String) extends Command[AlreadyOpen :+: CNil]
    case class Close(force: Boolean) extends Command[NotOpen :+: CNil]

    type s = Open :+: Close :+: CNil
  }

  object Event {
    case class Opened(owner: String)
    case class Closed()
    case class Balanced()

    type s = Opened :+: Closed :+: Balanced :+: CNil
  }

  object Handle extends CommandHandler[Account, Command.s, Event.s] {
    import Command._
    import Event._
    import Error._

    implicit val open = on[Open] { c ⇒ s ⇒
      if (!s.open) c.success(Opened(c.owner))
      else c.fail(AlreadyOpen())
    }

    implicit val close = on[Close] { c ⇒ s ⇒
      if (s.open) c.success(Closed() :: Balanced() :: HNil)
      else c.fail(NotOpen())
    }
  }

  object Apply extends EventApplicator[Account, Event.s] {
    import Event._
    implicit val opened = on[Opened] { e ⇒ s ⇒
      s.copy(open = true, owner = Some(e.owner))
    }
    implicit val closed = on[Closed] { e ⇒ s ⇒
      s.copy(open = false)
    }
    implicit val balanced = on[Balanced] { e ⇒ identity }
  }


  AggregateType[Id, Account, Command.s, Event.s](
    name = "Accout",
    seed = seed,
    handleCommand = handle,
    applyEvent = apply
  )
  private def seed(id: Id) = Account(id, None, false)
  private def handle(cmd: Command.s) = cmd.fold(Handle)
  private def apply(event: Event.s) = event.fold(Apply)

}
