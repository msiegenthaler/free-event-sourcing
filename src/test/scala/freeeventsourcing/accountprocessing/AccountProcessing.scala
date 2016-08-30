package freeeventsourcing.accountprocessing

import shapeless.{ ::, HNil }
import freeeventsourcing.accountprocessing.Account.Event.Closed
import freeeventsourcing.api.domainmodel.DomainModel

object AccountProcessing extends DomainModel {
  val name = "account processing"
  val aggregates = Account :: Transaction :: HNil
  type Aggregates = Account.type :: Transaction.type :: HNil
}