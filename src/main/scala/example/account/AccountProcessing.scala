package example.account

import scala.language.higherKinds
import shapeless._
import slfes.BoundedContextDefinition

/** Bounded context for account processing. */
object AccountProcessing {
  implicit val account = AccountAggregate.definition.interface
  implicit val transaction = TransactionAggregate.definition.interface

  val definition = BoundedContextDefinition("account processing",
    aggregates = AccountAggregate.definition :: TransactionAggregate.definition :: HNil,
    processes = BlockFundsProcess.definition :: TransactionResultProcess.definition :: HNil
  ).boundedContext
}