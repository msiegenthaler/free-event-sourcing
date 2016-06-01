package freeeventsourcing.accountprocessing

import freeeventsourcing.{ AggregateImplementation, BoundedContextImplementation }

package object impl {
  val accountProcessing = {
    implicit val account = AggregateImplementation(Account)(
      seed = AccountState.initial,
      applyEvent = AccountApplicator.function,
      handleCommand = AccountHandler
    )

    implicit val transaction = AggregateImplementation(Transaction)(
      seed = TransactionState.initial,
      applyEvent = TransactionApplicator,
      handleCommand = TransactionHandler
    )

    BoundedContextImplementation(
      AccountProcessing,
      BlockFundsProcess.process :: Nil
    )
  }
}