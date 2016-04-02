package slfes2.accountprocessing

import slfes2.{ AggregateImplementation, BoundedContextImplementation }

package object impl {
  val accountProcessing = {

    implicit val account = AggregateImplementation(Account)(
      seed = _ ⇒ AccountState(None, false),
      eventApplicator = AccountApplicator,
      commandHandler = AccountHandler
    )

    implicit val transaction = AggregateImplementation(Transaction)(
      seed = _ ⇒ TransactionState(),
      eventApplicator = TransactionApplicator,
      commandHandler = TransactionHandler
    )

    BoundedContextImplementation(AccountProcessing)
  }
}
