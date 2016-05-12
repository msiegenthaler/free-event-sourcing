package freeeventsourcing.support

import org.scalatest.{ FlatSpec, Matchers }
import shapeless.{ ::, HNil }
import freeeventsourcing.accountprocessing.{ Account, Transaction }

class AggregateFromIdTests extends FlatSpec with Matchers {
  val aggregates = Account :: Transaction :: HNil

  "AggregateFromId " should " find the first Aggregate instance from its Id type" in {
    AggregateFromId(aggregates)[Account.Id] shouldBe Account
  }

  "AggregateFromId " should " find the first Aggregate instance from an Id" in {
    AggregateFromId(aggregates)(Account.Id(1)) shouldBe Account
  }

  "AggregateFromId " should " find the second Aggregate instance from its Id type" in {
    AggregateFromId(aggregates)[Transaction.Id] shouldBe Transaction
  }

  "AggregateFromId " should " find the second Aggregate instance from an Id" in {
    AggregateFromId(aggregates)(Transaction.Id(1)) shouldBe Transaction
  }

  "AggregateFromId " should " fail to compile if no aggregate with this Id type is known" in {
    "AggregateFromId[String, Aggregates]" shouldNot compile
    "AggregateFromId[Transaction.Id, Account.type :: HNil]" shouldNot compile
  }

}
