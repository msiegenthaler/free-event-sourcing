package slfes2.accountprocessing

import shapeless.{ :+:, CNil, Coproduct }
import slfes2.{ Aggregate, AggregateCommand }

object Account extends Aggregate {
  val name = "Account"

  final case class Id(id: Long)

  sealed trait Event
  object Event {
    final case class Opened(owner: String) extends Event
    final case class Closed() extends Event
  }

  sealed trait Command extends AggregateCommand
  object Command {
    import Error._
    final case class Open(owner: String) extends Command {
      type Error = AlreadyOpen :+: CNil
    }
    final case class Close() extends Command {
      type Error = NotOpen :+: CNil
    }
  }

  object Error {
    final case class AlreadyOpen()
    final case class NotOpen()
  }
}