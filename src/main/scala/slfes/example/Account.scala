package slfes.example

import shapeless._
import slfes._
import slfes.utils.{CoproductFromBase, AllSingletons}

object Account {
  object Command {
    sealed trait Base extends Cmd
    import Error._
    case class Open(owner: String) extends Base {
      type Errors = AlreadyOpen :+: CNil
    }
    case class Close(force: Boolean) extends Base {
      type Errors = NotOpen :+: CNil
    }
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