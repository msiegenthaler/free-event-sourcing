package slfes2

import org.scalatest.{ FlatSpec, Matchers }
import slfes2.accountprocessing.Account.Event.{ Closed, Opened }
import slfes2.accountprocessing._

class AggregateEventTests extends FlatSpec with Matchers {
  val accountIndexer = AggregateEventIndexer(Account)

  val event1 = AggregateEvent[Account.type](Account, Account.Id(1), Opened("Mario"), MockEventTime(1))

  "AggregateEventIndexer " should " produce the same serialized form as the matching AggregateEventSelector" in {
    val toIndex = accountIndexer.apply(event1)
    toIndex.size shouldBe 1
    val indexed = toIndex.head

    val selector = AggregateEventSelector(Account)(Account.Id(1))[Opened]
    indexed shouldBe selector.serialize
  }

  "AggregateEventIndexer " should " produce the differend serialized form as a AggregateEventSelector that does not match the event type" in {
    val toIndex = accountIndexer.apply(event1)
    toIndex.size shouldBe 1
    val indexed = toIndex.head

    val selector = AggregateEventSelector(Account)(Account.Id(1))[Closed]
    indexed should not be (selector.serialize)
  }

  "AggregateEventIndexer " should " produce the differend serialized form as a AggregateEventSelector that does not match the aggregate" in {
    val toIndex = accountIndexer.apply(event1)
    toIndex.size shouldBe 1
    val indexed = toIndex.head

    val selector = AggregateEventSelector(Account)(Account.Id(2))[Opened]
    indexed should not be (selector.serialize)
  }

  "AggregateEventSelector " should " fail to compile if the base event class is used" in {
    "AggregateEventSelector(Account)(Account.Id(1))[Account.Event]" shouldNot compile
  }

  "AggregateEventSelector " should " fail to compile if an event is used that is not valid for the aggregate" in {
    "AggregateEventSelector(Account)(Account.Id(1))[String]" shouldNot compile
  }

  "AggregateEventSelector " should " be an instance of EventSelector" in {
    val selector = AggregateEventSelector(Account)(Account.Id(2))[Opened]
    def ser[S: EventSelector](s: S) = implicitly[EventSelector[S]].serialize(s)
    "ser(selector)" should compile
  }
}

case class MockEventTime(t: Int) extends EventTime