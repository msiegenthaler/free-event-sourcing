package example.account

import shapeless.{HNil, ::}
import slfes.{AggregateType, BoundedContextType}

/** Bounded context for account processing. */
object AccountProcessing {
  implicit val account = AccountAggregate.definition.interface
  implicit val transaction = TransactionAggregate.definition.interface

  val contextType = BoundedContextType("account processing",
    aggregates = AccountAggregate.definition :: TransactionAggregate.definition :: HNil,
    processes = BlockFundsProcess.definition :: TransactionResultProcess.definition :: HNil
  )


  val x = contextType.aggregates.map(AggregateType.ToAggregateInterface)
  val a: x.head.Id = Account.Id(1)
}
