package example.account

import shapeless.HNil
import slfes.BoundedContextType

/** Bounded context for account processing. */
object AccountProcessing {
  implicit val account = AccountAggregate.definition.aggregateType.interface
  implicit val transaction = TransactionAggregate.definition.aggregateType.interface

  val contextType = BoundedContextType("account processing",
    aggregates =
      AccountAggregate.definition :: TransactionAggregate.definition :: HNil,
    processes = BlockFundsProcess.definition :: TransactionResultProcess.definition :: HNil
  )
}
