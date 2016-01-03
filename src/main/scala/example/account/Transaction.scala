package example.account

import shapeless.{:+:, CNil}
import slfes.Cmd

/** A transaction transfers funds between two accounts. */
object Transaction {
  sealed trait Command extends Cmd
  object Command {
    import Error._
    type InvariantsViolated = Failed :+: CNil
    case class Create(from: Account.Id, to: Account.Id, amount: Amount) extends Command {
      type Errors = InvalidAmount :+: InvariantsViolated
    }
    case class Confirm() extends Command {
      type Errors = AlreadyCanceled :+: InvariantsViolated
    }
    case class Cancel() extends Command {
      type Errors = AlreadyConfirmed :+: InvariantsViolated
    }
  }

  sealed trait Event
  object Event {
    case class Created(from: Account.Id, to: Account.Id, amount: Amount) extends Event
    case class Confirmed(from: Account.Id, to: Account.Id, amount: Amount) extends Event
    case class Canceled(from: Account.Id, to: Account.Id) extends Event
  }

  object Error {
    case class InvalidAmount()
    case class AlreadyCanceled()
    case class AlreadyConfirmed()
    case class Failed(reason: String)
  }

  case class Id(id: Long)
}