package example.account

import shapeless._
import slfes.BoundedContextType

/** Bounded context for account processing. */
object AccountProcessing {
  implicit val account = AccountAggregate.definition.interface
  implicit val transaction = TransactionAggregate.definition.interface

  val contextType = BoundedContextType("account processing",
    aggregates =
      AccountAggregate.definition :: TransactionAggregate.definition :: HNil,
    processes = BlockFundsProcess.definition :: TransactionResultProcess.definition :: HNil
  )
}
