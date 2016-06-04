package freeeventsourcing.accountprocessing

import shapeless.{ ::, HNil }
import freeeventsourcing.accountprocessing.Account.Event.Closed
import freeeventsourcing.BoundedContext

object AccountProcessing extends BoundedContext {
  val name = "account processing"
  val aggregates = Account :: Transaction :: HNil
  type Aggregates = Account.type :: Transaction.type :: HNil
}