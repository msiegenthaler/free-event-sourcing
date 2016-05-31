package freeeventsourcing

import java.time.Instant
import java.util.concurrent.atomic.AtomicLong
import freeeventsourcing.EventTime.Zero
import org.scalatest.{ FlatSpec, Matchers }

class EventTests extends FlatSpec with Matchers {
  case class MyEvent(name: String)

  "EventWithMetadata " should " have implicit conversion to .payload" in {
    val ewm = EventWithMetadata(MyEvent("Mario"), EventMetadata(MockEventId(), MockEventTime()))
    ewm.payload.name shouldBe "Mario"
    ewm.name shouldBe "Mario"
  }

  val time1 = MockEventTime()
  val time2 = MockEventTime()

  "EventTime " should " have a notion of 'happened before'" in {
    time1.before(time2) shouldBe true
    time2.before(time2) shouldBe false
    time2.before(time1) shouldBe false
    time1.before(time1) shouldBe false
  }

  "EventTime " should " have a notion of 'happened after'" in {
    time2.after(time1) shouldBe true
    time1.after(time2) shouldBe false
    time2.after(time2) shouldBe false
    time1.after(time1) shouldBe false
  }

  "EventTime" should " have a Zero that is before any other time" in {
    Zero.before(time1) shouldBe true
    Zero.before(time2) shouldBe true
    Zero.before(Zero) shouldBe false
    time1.before(Zero) shouldBe false
    time2.before(Zero) shouldBe false
  }

  "EventTime.earlier " should " combine Zero with any to Zero" in {
    Zero.earlier(time1) shouldBe Zero
    Zero.earlier(time2) shouldBe Zero
    Zero.earlier(Zero) shouldBe Zero
    time1.earlier(Zero) shouldBe Zero
    time2.earlier(Zero) shouldBe Zero
  }

  "EventTime.later " should " combine Zero with X to X" in {
    Zero.later(time1) shouldBe time1
    Zero.later(time2) shouldBe time2
    Zero.later(Zero) shouldBe Zero
    time1.later(Zero) shouldBe time1
    time2.later(Zero) shouldBe time2
  }
}

case class MockEventId() extends EventId
case class MockEventTime(t: Long = MockEventTime.timer.incrementAndGet) extends EventTime {
  val when: Instant = Instant.now()
  def before(other: EventTime) = other match {
    case Zero              ⇒ false
    case MockEventTime(t2) ⇒ t < t2
  }
  def after(other: EventTime) = other match {
    case Zero              ⇒ true
    case MockEventTime(t2) ⇒ t > t2
  }
  def earlier(other: EventTime) = other match {
    case Zero              ⇒ Zero
    case MockEventTime(t2) ⇒ MockEventTime(t min t2)
  }
  def later(other: EventTime) = other match {
    case Zero              ⇒ this
    case MockEventTime(t2) ⇒ MockEventTime(t max t2)
  }
}
object MockEventTime {
  private val timer = new AtomicLong(0)
}
