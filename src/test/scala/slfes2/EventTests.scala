package slfes2

import scala.annotation.implicitNotFound
import slfes.utils.StringSerializable.ops._
import slfes.utils.{ =!=, StringSerializable }
import slfes2.accountprocessing.Account.Event.Opened
import slfes2.accountprocessing._
import slfesakka.CompositeName

object EventTests {

}

object Usage {
  val aggregateevent1 = AggregateEvent[Account.type](Account, Account.Id(1), Opened("Mario"))
  val event1 = Event(???, aggregateevent1)

  class IndexedAggregateEvent[A <: Aggregate](implicit s: StringSerializable[A#Id]) extends IndexedEvent[AggregateEvent[A]] {
    def asString(key: AggregateEvent[A]) = {
      val eventType = "123"
      val name = CompositeName.root / key.aggregateType.name / key.aggregate.serializeToString / eventType
      name.serialize
    }
  }

  val selector = AggregateEventSelector(Account)(Account.Id(1))[Opened]

  //  AggregateEventSelector(Account)(Account.Id(1))[Account.Event] //must fail to compile

  //  object Indexer1 extends EventIndexer {
  //    def apply(e: Event[_]) = e match {
  //      case Event(_, e: AggregateEvent[_]) ⇒
  //        implicit val a: StringSerializable[e.aggregateType.type#Id] = ???
  //        new IndexedAggregateEvent[e.aggregateType.type]() :: Nil
  //      case _ ⇒ Nil
  //    }
  //  }

}