package slfes2.accountprocessing

import slfes2.{ AggregateImplementation, BoundedContextImplementation }

package object impl {
  val accountProcessing = {
    implicit val account = AccountImplementation
    implicit val transaction = TransactionImplementation
    BoundedContextImplementation(AccountProcessing)
  }
}
