package slfes2

import shapeless.Typeable
import slfes.utils.StringSerializable
import slfes2.accountprocessing.Account.Event.Opened
import slfes2.accountprocessing._

object EventTests {

}

object Usage {
  val t0: EventTime = ???
  val event1 = AggregateEvent[Account.type](Account, Account.Id(1), Opened("Mario"), t0)

  def aggregateEventIndexer[A <: Aggregate](aggregate: A)(implicit t: Typeable[AggregateEvent[A]], idser: StringSerializable[A#Id]) = { event: Any ⇒
    t.cast(event).filter(_.aggregateType == aggregate).map { event ⇒
      val eventType = event.event.getClass.getName //TODO change to more robust implementation
      val selector = AggregateEventSelector(aggregate, event.aggregate, eventType)
      selector.serialize
    }
  }

  val selector = AggregateEventSelector(Account)(Account.Id(1))[Opened]
  //  AggregateEventSelector(Account)(Account.Id(1))[Account.Event] //must fail to compile
}