package freeeventsourcing

import org.scalatest.{ FlatSpec, Matchers }
import shapeless.{ ::, HNil }
import freeeventsourcing.EventSelector.WithEventType
import freeeventsourcing.EventSelector.ops._
import freeeventsourcing.accountprocessing.Account.Event.{ Closed, Opened }
import freeeventsourcing.accountprocessing.Transaction.Event.Created
import freeeventsourcing.accountprocessing._
import freeeventsourcing.utils.CompositeName

class AggregateEventTests extends FlatSpec with Matchers {
  val accountIndexer = AggregateEventRouter(Account)

  val event1 = AggregateEvent[Account.type](Account, Account.Id(1), Opened("Mario"), MockEventTime(1))

  "AggregateEventRouter " should " produce the same topic as the matching AggregateEventSelector" in {
    val toIndex = accountIndexer.apply(event1)
    toIndex.size shouldBe 1
    val indexed = toIndex.head

    val selector = AggregateEventSelector(Account)(Account.Id(1))[Opened]
    indexed shouldBe selector.topic
  }

  "AggregateEventRouter " should " produce a different topic than an AggregateEventSelector that does not match the event type" in {
    val toIndex = accountIndexer.apply(event1)
    toIndex.size shouldBe 1
    val indexed = toIndex.head

    val selector = AggregateEventSelector(Account)(Account.Id(1))[Closed]
    indexed should not be (selector.topic)
  }

  "AggregateEventRouter " should " produce a different topic than an AggregateEventSelector that does not match the aggregate" in {
    val toIndex = accountIndexer.apply(event1)
    toIndex.size shouldBe 1
    val indexed = toIndex.head

    val selector = AggregateEventSelector(Account)(Account.Id(2))[Opened]
    indexed should not be (selector.topic)
  }

  "AggregateEventSelector " should " fail to compile if the base event class is used" in {
    "AggregateEventSelector(Account)(Account.Id(1))[Account.Event]" shouldNot compile
  }

  "AggregateEventSelector " should " fail to compile if an event is used that is not valid for the aggregate" in {
    "AggregateEventSelector(Account)(Account.Id(1))[String]" shouldNot compile
  }

  "AggregateEventSelector " should " be an instance of EventSelector" in {
    val selector = AggregateEventSelector(Account)(Account.Id(2))[Opened]
    def ser[S <: WithEventType: EventSelector](s: S) = implicitly[EventSelector[S]].topic(s)
    "ser(selector)" should compile
  }

  "AggregateEventSelector.ValidFor " should " have an instance for subtypes of events of first aggregate in list" in {
    type L = Account.type :: Transaction.type :: HNil
    "implicitly[AggregateEventSelector.ValidFor[AggregateEventSelector[Account.type, Opened], L]]" should compile
    "implicitly[AggregateEventSelector.ValidFor[AggregateEventSelector[Account.type, Closed], L]]" should compile
  }

  "AggregateEventSelector.ValidFor " should " have an instance for subtypes of events of second (and last) aggregate in list" in {
    type L = Account.type :: Transaction.type :: HNil
    "implicitly[AggregateEventSelector.ValidFor[AggregateEventSelector[Transaction.type, Created], L]]" should compile
  }

  "AggregateEventSelector.ValidFor " should " have no instance for events of aggregate not list" in {
    type L = Account.type :: HNil
    "implicitly[AggregateEventSelector.ValidFor[AggregateEventSelector[Transaction.type, Created], L]]" shouldNot compile
  }

  "AggregateEventSelector.select " should " match a correct event" in {
    val s = AggregateEventSelector(Account)(Account.Id(2))[Opened]
    s.select(Opened("Mario")) shouldBe Some(Opened("Mario"))
  }

  "AggregateEventSelector.select " should " not match a wrong event" in {
    val s = AggregateEventSelector(Account)(Account.Id(2))[Opened]
    s.select(Closed) shouldBe None
  }

  "AggregateEventSelector.topic " should " be under aggregate and contain the type and the id" in {
    val s = AggregateEventSelector(Account)(Account.Id(2))[Opened]
    val expected = CompositeName.root / "aggregate" / "Account" / "2" / "freeeventsourcing.accountprocessing.Account$Event$Opened"
    s.topic shouldBe EventTopic(expected)
  }
}

case class MockEventTime(t: Int) extends EventTime