package example.account

import shapeless.{Generic, :+:, CNil}
import slfes.Cmd
import slfes.utils.CoproductFromBase

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
  object Commands extends CoproductFromBase(Generic[Command])

  sealed trait Event
  object Event {
    case class Created(from: Account.Id, to: Account.Id, amount: Amount) extends Event
    case class Confirmed(from: Account.Id, to: Account.Id, amount: Amount) extends Event
    case class Canceled(from: Account.Id, to: Account.Id) extends Event
  }
  object Events extends CoproductFromBase(Generic[Event])

  object Error {
    case class InvalidAmount()
    case class AlreadyCanceled()
    case class AlreadyConfirmed()
    case class Failed(reason: String)
  }

  case class Id(id: Long)
}