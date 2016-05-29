package freeeventsourcing

import org.scalatest.{ FlatSpec, Matchers }

class EventTests extends FlatSpec with Matchers {
  case class MyEvent(name: String)

  "EventWithMetadata " should " have implicit conversion to .payload" in {
    val ewm = EventWithMetadata(MyEvent("Mario"), EventMetadata(MockEventId(), MockEventTime()))
    ewm.payload.name shouldBe "Mario"
    ewm.name shouldBe "Mario"
  }
}

case class MockEventId() extends EventId
case class MockEventTime() extends EventTime
