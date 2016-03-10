package example.account

import shapeless.{ :+:, CNil }
import slfes.Cmd

/** A transaction transfers funds between two accounts. */
object Transaction {
  object Command {
    import Error._
    type InvariantsViolated = Failed :+: CNil
    case class Create(from: Account.Id, to: Account.Id, amount: Amount) extends Cmd {
      type Errors = InvalidAmount :+: InvariantsViolated
    }
    case class Confirm() extends Cmd {
      type Errors = AlreadyCanceled :+: InvariantsViolated
    }
    case class Cancel() extends Cmd {
      type Errors = AlreadyConfirmed :+: InvariantsViolated
    }
  }

  object Event {
    case class Created(from: Account.Id, to: Account.Id, amount: Amount)
    case class Confirmed(from: Account.Id, to: Account.Id, amount: Amount)
    case class Canceled(from: Account.Id, to: Account.Id)
  }

  object Error {
    case class InvalidAmount()
    case class AlreadyCanceled()
    case class AlreadyConfirmed()
    case class Failed(reason: String)
  }

  case class Id(id: Long)
}