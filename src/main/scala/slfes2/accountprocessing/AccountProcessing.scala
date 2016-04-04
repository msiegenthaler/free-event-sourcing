package slfes2.accountprocessing

import shapeless.{ HNil, :: }
import slfes2.{ BoundedContext }

object AccountProcessing extends BoundedContext {
  val name = "account processing"
  val aggregates = Account :: Transaction :: HNil
  type Aggregates = Account.type :: Transaction.type :: HNil
}