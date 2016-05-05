package slfes2.accountprocessing

import shapeless.{ ::, HNil }
import slfes2.accountprocessing.Account.Event.Closed
import slfes2.{ AggregateEventSelector, BoundedContext }

object AccountProcessing extends BoundedContext {
  val name = "account processing"
  val aggregates = Account :: Transaction :: HNil
  type Aggregates = Account.type :: Transaction.type :: HNil
}