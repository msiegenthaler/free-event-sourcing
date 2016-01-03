package example.account

import shapeless.HNil
import slfes.BoundedContextType

/** Bounded context for account processing. */
object AccountProcessing {
  implicit val account = AccountAggregate.definition.aggregate
  implicit val transaction = TransactionAggregate.definition.aggregate

  val contextType = BoundedContextType("account processing",
    aggregates =
      AccountAggregate.definition :: TransactionAggregate.definition :: HNil,
    processes = BlockFundsProcess.definition :: TransactionResultProcess.definition :: HNil
  )
}
