package slfes

import shapeless._


object Example {

  object Command {
    case class Open(owner: String) extends Command[CNil]
    case class Close(force: Boolean) extends Command[String :+: CNil]

    type s = Open :+: Close :+: CNil
  }

  object Handler extends CommandHandler[Command.s] {
    import Command._

    implicit val open = on[Open] { c ⇒
      c.success
    }

    implicit val close = on[Close] { c ⇒
      if (c.force) c.fail("failed, too much force used")
      else c.success
    }
  }

  def handle(cmd: Command.s) = cmd.fold(Handler)
}
