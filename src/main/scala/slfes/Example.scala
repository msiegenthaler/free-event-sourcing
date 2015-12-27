package slfes

import shapeless._


object Example {

  object Command {
    case class Open(owner: String) extends Command[CNil]
    case class Close(force: Boolean) extends Command[String :+: CNil]

    type s = Open :+: Close :+: CNil
  }

  object Event {
    case class Opened(owner: String)
    case class Closed()
    case class Balanced()

    type s = Opened :+: Closed :+: Balanced :+: CNil
  }

  object Handler extends CommandHandler[Command.s, Event.s] {
    import Command._
    import Event._

    implicit val open = on[Open] { c ⇒
      c.success(Opened(c.owner))
    }

    implicit val close = on[Close] { c ⇒
      if (c.force) c.fail("failed, too much force used")
      else c.success(Closed() :: Balanced() :: HNil)
    }
  }

  def handle(cmd: Command.s) = cmd.fold(Handler)
}
