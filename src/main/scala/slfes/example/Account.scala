package slfes.example

import shapeless._
import slfes._
import slfes.utils.CoproductFromBase

object Account {
  sealed trait Command extends Cmd
  object Command {
    import Error._
    type Err = Failed :+: CNil

    case class Open(owner: String) extends Command {
      type Errors = AlreadyOpen :+: Err
    }
    case class Close(force: Boolean) extends Command {
      type Errors = NotOpen :+: Err
    }
  }
  object Commands extends CoproductFromBase(Generic[Command])

  object Error {
    case class AlreadyOpen()
    case class NotOpen()
    case class Failed(reason: String)
  }

  sealed trait Event
  object Event {
    case class Opened(owner: String) extends Event
    case class Closed() extends Event
    case class Balanced() extends Event
  }
  object Events extends CoproductFromBase(Generic[Event])

  case class Id(id: Long)
  case class State(id: Id, owner: Option[String], open: Boolean)
}