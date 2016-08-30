package freeeventsourcing.accountprocessing

import freeeventsourcing.api.domainmodel.{ Aggregate, AggregateCommand, DomainEvent }
import shapeless.{ :+:, CNil }

object Transaction extends Aggregate {
  val name = "Transaction"

  final case class Id(id: Long)

  sealed trait Event extends DomainEvent
  object Event {
    final case class Created(from: Account.Id, to: Account.Id, amount: Long) extends Event
    final case class Confirmed() extends Event
    final case class Canceled() extends Event
  }

  sealed trait Command extends AggregateCommand
  object Command {
    import Error._
    final case class Create(from: Account.Id, to: Account.Id, amount: Long) extends Command {
      type Error = AlreadyExists :+: CNil
    }
    final case class Confirm() extends Command {
      type Error = AlreadyCanceled :+: DoesNotExist :+: CNil
    }
    final case class Cancel() extends Command {
      type Error = AlreadyConfirmed :+: DoesNotExist :+: CNil
    }
  }

  object Error {
    final case class AlreadyExists()
    final case class DoesNotExist()
    final case class AlreadyConfirmed()
    final case class AlreadyCanceled()
  }
}