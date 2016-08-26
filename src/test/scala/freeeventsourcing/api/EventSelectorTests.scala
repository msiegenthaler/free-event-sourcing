package freeeventsourcing.api

import freeeventsourcing.accountprocessing.Account
import freeeventsourcing.accountprocessing.Account.Event.Opened
import freeeventsourcing.api.EventTime.Zero
import org.scalatest.{ FlatSpec, Matchers }
import ProcessActionTests._
import EventSelector.ops._

class EventSelectorTests extends FlatSpec with Matchers {
  val selectorMario = selectorOpened.where { (e, m) ⇒
    if (e.event.owner == "Mario") Some(e)
    else None
  }

  val t1 = MockEventTime()
  val t2 = MockEventTime()
  val t3 = MockEventTime()
  val selectorT1 = selectorOpened.after(t1)
  val selectorT2 = selectorOpened.after(t2)

  def mockMetadata = EventMetadata(MockEventId(), MockEventTime())
  val openedForMario = AggregateEvent.create(Account)(Account.Id(1), Opened("Mario"))
  val openedForMaya = AggregateEvent.create(Account)(Account.Id(1), Opened("Maya"))

  "EventSelector " should " let pass matching events" in {
    selectorOpened.select(openedForMario, mockMetadata) shouldBe Some(openedForMario)
  }

  "EventSelector.where " should " let pass matching events" in {
    selectorMario.select(openedForMario, mockMetadata) shouldBe Some(openedForMario)
  }

  "EventSelector.where " should " not let pass non-matching events" in {
    selectorMario.select(openedForMaya, mockMetadata) shouldBe None
  }

  "EventSelector.where " should " have same topic as the underlying" in {
    selectorMario.topic shouldBe selectorOpened.topic
  }

  "EventSelector.filter " should " let pass matching events" in {
    selectorOpened.filter(_.event.owner == "Mario").select(openedForMario, mockMetadata) shouldBe Some(openedForMario)
  }

  "EventSelector.filter " should " not let pass non-matching events" in {
    selectorMario.filter(_.event.owner == "Mario").select(openedForMaya, mockMetadata) shouldBe None
  }

  "EventSelector.filter " should " have same topic as the underlying" in {
    selectorMario.filter(_.event.owner == "Mario").topic shouldBe selectorOpened.topic
  }

  "EventSelector.after(t1) " should " change the startTime to t1 for selectors that had Zero" in {
    selectorOpened.startTime shouldBe Zero
    val s = selectorOpened.after(t1)
    s.startTime shouldBe t1
  }

  "EventSelector.after(t2) " should " change the startTime to t1.later(t2) for selectors that had t1" in {
    selectorT1.startTime shouldBe t1
    val s = selectorT1.after(t2)
    s.startTime shouldBe t1.later(t2)
  }
  "EventSelector.after(t1) " should " change the startTime to t1.later(t2) for selectors that had t2" in {
    selectorT2.startTime shouldBe t2
    val s = selectorT2.after(t1)
    println(selectorT2)
    println(s)
    s.startTime shouldBe t1.later(t2)
  }

  "EventSelector.after(t2)" should " ignore events that happened before or at t2" in {
    selectorOpened.after(t2).select(openedForMario, EventMetadata(MockEventId(), Zero)) shouldBe None
    selectorOpened.after(t2).select(openedForMario, EventMetadata(MockEventId(), t1)) shouldBe None
    selectorOpened.after(t2).select(openedForMario, EventMetadata(MockEventId(), t2)) shouldBe None
  }

  "EventSelector.after(t2)" should " let pass events that happened after t2" in {
    selectorOpened.after(t2).select(openedForMario, EventMetadata(MockEventId(), t3)) shouldBe Some(openedForMario)
  }

  "EventSelector.after(Zero)" should " accept all events" in {
    selectorOpened.after(Zero).select(openedForMario, EventMetadata(MockEventId(), t1)) shouldBe Some(openedForMario)
    selectorOpened.after(Zero).select(openedForMario, EventMetadata(MockEventId(), t2)) shouldBe Some(openedForMario)
    selectorOpened.after(Zero).select(openedForMario, EventMetadata(MockEventId(), t3)) shouldBe Some(openedForMario)
  }
}
