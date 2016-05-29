package freeeventsourcing

import freeeventsourcing.ProcessActionTests.{ OtherEvent, TestAggregate }
import freeeventsourcing.accountprocessing.Account.Event.Opened
import freeeventsourcing.accountprocessing._
import org.scalatest.{ FlatSpec, Matchers }

class AggregateEventTests extends FlatSpec with Matchers {
  "AggregateEvent " should " have eventType equal to the relative classname of the event within the aggregate" in {
    val e = AggregateEvent[Account.type, Opened](Account, Account.Id(1), Opened("Mario"))
    e.eventType shouldBe "Opened"
  }

  "AggregateEvent " should " have eventType equal to the absolute classname of the event when not nested in the aggregate" in {
    val e = AggregateEvent[TestAggregate.type, OtherEvent](TestAggregate, TestAggregate.Id(1), OtherEvent())
    e.eventType shouldBe "freeeventsourcing.ProcessActionTests$OtherEvent"
  }
}