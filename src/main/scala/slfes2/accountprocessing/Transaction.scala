package slfes2.accountprocessing

import shapeless.{ :+:, CNil, Coproduct }
import slfes2.{ Aggregate, AggregateCommand }

object Transaction extends Aggregate {
  val name = "Transaction"

  final case class Id(id: Long)

  sealed trait Event
  object Event {
    final case class Created(from: Account.Id, to: Account.Id, amount: Long) extends Event
    final case class Commited() extends Event
  }

  sealed trait Command extends AggregateCommand
  object Command {
    import Error._
    final case class Create(from: Account.Id, to: Account.Id, amount: Long) extends Command {
      type Error = AlreadyExists :+: CNil
    }
    final case class Commit() extends Command {
      type Error = DoesNotExist :+: CNil
    }
  }

  object Error {
    final case class AlreadyExists()
    final case class DoesNotExist()
  }
}