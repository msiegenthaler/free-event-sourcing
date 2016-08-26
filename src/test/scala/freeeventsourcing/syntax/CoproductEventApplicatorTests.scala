package freeeventsourcing.syntax

import freeeventsourcing.api.DomainEvent
import org.scalatest.{ FlatSpec, Matchers }

class CoproductEventApplicatorTests extends FlatSpec with Matchers {
  sealed trait Event extends DomainEvent
  object Event {
    final case class Event1(a: String) extends Event
    final case class Event2(a: String) extends Event
  }
  import Event._

  val e1: Event = Event1("Mario")
  val e2: Event = Event2("Peter")

  "CoproductEventApplicator " should " allow handling events" in {
    object H extends CoproductEventApplicator[Event, String] {
      implicit val event1 = on[Event1] { e ⇒ s ⇒ s + e.a }
      implicit val event2 = on[Event2] { e ⇒ s ⇒ s + e.a + "!" }
    }
    H.function.apply(e1, "Hi ") shouldBe "Hi Mario"
    H.function.apply(e2, "Hi ") shouldBe "Hi Peter!"
  }

  "CoproductEventApplicator " should " fail to compile if not all events are handled" in {
    """
      |    object H extends CoproductEventApplicator[Event, String] {
      |      implicit val event1 = on[Event1] { e ⇒ s ⇒ s + e.a }
      |    }
      |    H.function.apply(e1, "Hi ")
    """.stripMargin shouldNot compile
  }

  "CoproductEventApplicator " should " fail to compile if no events are handled" in {
    """
      |    object H extends CoproductEventApplicator[Event, String] {}
      |    H.function.apply(e1, "Hi ")
    """.stripMargin shouldNot compile
  }
}
