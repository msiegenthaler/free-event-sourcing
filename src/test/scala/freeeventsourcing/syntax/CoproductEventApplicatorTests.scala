package freeeventsourcing.syntax

import org.scalatest.{ FlatSpec, Matchers }

class CoproductEventApplicatorTests extends FlatSpec with Matchers {
  sealed trait Event
  object Event {
    case class Event1(a: String) extends Event
    case class Event2(a: String) extends Event
  }
  import Event._

  "CoproductEventApplicator " should " allow handling events" in {
    object H extends CoproductEventApplicator[Event, String] {
      implicit val event1 = on[Event1] { e ⇒ s ⇒ s + e.a }
      implicit val event2 = on[Event2] { e ⇒ s ⇒ s + e.a + "!" }
    }
    H(Event1("Mario")).apply("Hi ") shouldBe "Hi Mario"
    H(Event2("Peter")).apply("Hi ") shouldBe "Hi Peter!"
  }

  "CoproductEventApplicator " should " fail to compile if not all events are handled" in {
    """
      |    object H extends CoproductEventApplicator[Event, String] {
      |      implicit val event1 = on[Event1] { e ⇒ s ⇒ s + e.a }
      |    }
      |    H(Event1("Mario"): Event)
    """.stripMargin shouldNot compile
  }

  "CoproductEventApplicator " should " fail to compile if no events are handled" in {
    """
      |    object H extends CoproductEventApplicator[Event, String] {}
      |    H(Event1("Mario"): Event)
    """.stripMargin shouldNot compile
  }
}
