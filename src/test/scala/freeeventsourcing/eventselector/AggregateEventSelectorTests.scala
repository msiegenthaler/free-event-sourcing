package freeeventsourcing.eventselector

import freeeventsourcing.EventSelector.WithEventType
import freeeventsourcing.EventSelector.ops._
import freeeventsourcing.accountprocessing.Account.Event.{ Closed, Opened }
import freeeventsourcing.accountprocessing.Transaction.Event.Created
import freeeventsourcing.accountprocessing._
import freeeventsourcing.utils.CompositeName
import freeeventsourcing._
import org.scalatest.{ FlatSpec, Matchers }
import shapeless.{ ::, HNil }

class AggregateEventSelectorTests extends FlatSpec with Matchers {
  val router = AggregateEventSelector.Router.forAggregateType(Account)

  def mockMetadata = EventMetadata(MockEventId(), MockEventTime())
  val openedEvent = EventWithMetadata(AggregateEvent[Account.type, Opened](Account, Account.Id(1), Opened("Mario")), mockMetadata)

  "AggregateEventSelector.Router " should " produce the same topic as the matching AggregateEventSelector" in {
    val toIndex = router.apply(openedEvent)
    toIndex.size shouldBe 1
    val indexed = toIndex.head

    val selector = AggregateEventSelector(Account)(Account.Id(1))[Opened]
    indexed shouldBe selector.topic
  }

  "AggregateEventSelector.Router " should " produce a different topic than an AggregateEventSelector that does not match the event type" in {
    val toIndex = router.apply(openedEvent)
    toIndex.size shouldBe 1
    val indexed = toIndex.head

    val selector = AggregateEventSelector(Account)(Account.Id(1))[Closed]
    indexed should not be (selector.topic)
  }

  "AggregateEventSelector.Router " should " produce a different topic than an AggregateEventSelector that does not match the aggregate" in {
    val toIndex = router.apply(openedEvent)
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
    val s = AggregateEventSelector(Account)(Account.Id(1))[Opened]
    s.select(openedEvent) shouldBe Some(openedEvent)
  }

  "AggregateEventSelector.select " should " not match a wrong event type" in {
    val s = AggregateEventSelector(Account)(Account.Id(1))[Closed]
    s.select(EventWithMetadata(Opened("Mario"), mockMetadata)) shouldBe None
    s.select(EventWithMetadata(Closed(), mockMetadata)) shouldBe None
    s.select(openedEvent) shouldBe None
  }

  "AggregateEventSelector.select " should " not match a wrong aggregate" in {
    val s = AggregateEventSelector(Account)(Account.Id(0))[Opened]
    s.select(openedEvent) shouldBe None
  }

  "AggregateEventSelector.select " should " not match a wrong aggregate type" in {
    val s = AggregateEventSelector(Transaction)(Transaction.Id(1))[Created]
    s.select(openedEvent) shouldBe None
  }

  "AggregateEventSelector.topic " should " be under aggregate and contain the type and the id" in {
    val s = AggregateEventSelector(Account)(Account.Id(1))[Opened]
    val expected = CompositeName.root / "aggregate" / "Account" / "1" / "Opened"
    s.topic shouldBe EventTopic(expected)
  }
}