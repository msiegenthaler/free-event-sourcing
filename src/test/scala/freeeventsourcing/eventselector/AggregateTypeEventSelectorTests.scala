package freeeventsourcing.eventselector

import freeeventsourcing._
import freeeventsourcing.EventSelector.WithEventType
import freeeventsourcing.EventSelector.ops._
import freeeventsourcing.accountprocessing.Account.Event.{ Closed, Opened }
import freeeventsourcing.accountprocessing.Transaction.Event.Created
import freeeventsourcing.accountprocessing.{ Account, Transaction }
import freeeventsourcing.utils.CompositeName
import org.scalatest.{ FlatSpec, Matchers }

class AggregateTypeEventSelectorTests extends FlatSpec with Matchers {
  val router = AggregateTypeEventSelector.Router.forAggregateType(Account)

  def mockMetadata = EventMetadata(MockEventId(), MockEventTime())
  val openedEvent = EventWithMetadata(AggregateEvent[Account.type, Opened](Account, Account.Id(1), Opened("Mario")), mockMetadata)

  "AggregateTypeEventSelector.Router " should " produce the same topic as the matching AggregateTypeEventSelector" in {
    val toIndex = router.apply(openedEvent)
    toIndex.size shouldBe 1
    val indexed = toIndex.head

    val selector = AggregateTypeEventSelector(Account)[Opened]
    indexed shouldBe selector.topic
  }

  "AggregateTypeEventSelector.Router " should " produce a different topic than an AggregateTypeEventSelector that does not match the event type" in {
    val toIndex = router.apply(openedEvent)
    toIndex.size shouldBe 1
    val indexed = toIndex.head

    val selector = AggregateTypeEventSelector(Account)[Closed]
    indexed should not be (selector.topic)
  }

  "AggregateTypeEventSelector.Router " should " produce a different topic than an AggregateTypeEventSelector that does not match the aggregate type" in {
    val toIndex = router.apply(openedEvent)
    toIndex.size shouldBe 1
    val indexed = toIndex.head

    object Bla extends Aggregate {
      val name = "Bla"
      case class Id()
      sealed trait Command extends AggregateCommand
      sealed trait Event
      object Event {
        case class Opened(owner: String) extends Event
      }

    }

    val selector = AggregateTypeEventSelector(Bla)[Bla.Event.Opened]
    indexed should not be (selector.topic)
  }

  "AggregateTypeEventSelector " should " fail to compile if the base event class is used" in {
    "AggregateTypeEventSelector(Account)[Account.Event]" shouldNot compile
  }

  "AggregateTypeEventSelector " should " fail to compile if an event is used that is not valid for the aggregate" in {
    "AggregateTypeEventSelector(Account)[String]" shouldNot compile
  }

  "AggregateTypeEventSelector " should " be an instance of EventSelector" in {
    val selector = AggregateTypeEventSelector(Account)[Opened]
    def ser[S <: WithEventType: EventSelector](s: S) = implicitly[EventSelector[S]].topic(s)
    "ser(selector)" should compile
  }

  "AggregateTypeEventSelector.select " should " match a correct event" in {
    val s = AggregateTypeEventSelector(Account)[Opened]
    s.select(openedEvent) shouldBe Some(openedEvent)
  }

  "AggregateTypeEventSelector.select " should " not match a wrong event type" in {
    val s = AggregateTypeEventSelector(Account)[Closed]
    s.select(EventWithMetadata(Opened("Mario"), mockMetadata)) shouldBe None
    s.select(EventWithMetadata(Closed(), mockMetadata)) shouldBe None
    s.select(openedEvent) shouldBe None
  }

  "AggregateTypeEventSelector.select " should " not match a wrong aggregate type" in {
    val s = AggregateTypeEventSelector(Transaction)[Created]
    s.select(openedEvent) shouldBe None
  }

  "AggregateTypeEventSelector.topic " should " be under aggregate and contain the type" in {
    val s = AggregateTypeEventSelector(Account)[Opened]
    val expected = CompositeName.root / "aggregateType" / "Account" / "Opened"
    s.topic shouldBe EventTopic(expected)
  }
}