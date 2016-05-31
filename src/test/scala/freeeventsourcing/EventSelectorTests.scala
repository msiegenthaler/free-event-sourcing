package freeeventsourcing

import org.scalatest.{ FlatSpec, Matchers }
import freeeventsourcing.EventSelector.ops._
import freeeventsourcing.ProcessActionTests.selectorOpened
import freeeventsourcing.accountprocessing.Account
import freeeventsourcing.accountprocessing.Account.Event.Opened

class EventSelectorTests extends FlatSpec with Matchers {
  val selectorMario = selectorOpened.where { (e, m) ⇒
    if (e.event.owner == "Mario") Some(e)
    else None
  }

  def mockMetadata = EventMetadata(MockEventId(), MockEventTime())
  val openedForMario = EventWithMetadata(AggregateEvent.create(Account)(Account.Id(1), Opened("Mario")), mockMetadata)
  val openedForMaya = EventWithMetadata(AggregateEvent.create(Account)(Account.Id(1), Opened("Maya")), mockMetadata)

  "EventSelector " should " let pass matching events" in {
    selectorOpened.select(openedForMario) shouldBe Some(openedForMario.payload)
  }

  "EventSelector.where " should " let pass matching events" in {
    selectorMario.select(openedForMario) shouldBe Some(openedForMario.payload)
  }

  "EventSelector.where " should " not let pass non-matching events" in {
    selectorMario.select(openedForMaya) shouldBe None
  }

  "EventSelector.where " should " have same topic as the underlying" in {
    selectorMario.topic shouldBe selectorOpened.topic
  }
}
