package slfes2.accountprocessing

import slfes2.{ AggregateImplementation, BoundedContextImplementation }

package object impl {
  val accountProcessing = {
    implicit val account = AggregateImplementation(Account)(
      seed = _ ⇒ AccountState(None, false),
      applyEvent = AccountApplicator.function,
      handleCommand = AccountHandler
    )

    implicit val transaction = AggregateImplementation(Transaction)(
      seed = _ ⇒ TransactionState(None, false),
      applyEvent = TransactionApplicator,
      handleCommand = TransactionHandler
    )

    BoundedContextImplementation(AccountProcessing)
  }
}
