package slfes2

import org.scalatest.{ FlatSpec, Matchers }
import shapeless.{ ::, HNil }
import slfes2.Process.ValidAggregateSelector
import slfes2.accountprocessing.{ Account, AccountProcessing, Transaction }
import slfes2.accountprocessing.Account.Event.{ Closed, Opened }
import slfes2.accountprocessing.Transaction.Event.Created

class ProcessTests extends FlatSpec with Matchers {
  object TestAggregate extends Aggregate {
    val name = "Test Aggregate"
    case class Id(id: Int)
    sealed trait Event
    object Event {
      case class MyEvent(value: String) extends Event
    }
    sealed trait Command extends AggregateCommand
  }

  val selectorOpened = AggregateEventSelector(Account)(Account.Id(1))[Opened]
  val selectorClosed = AggregateEventSelector(Account)(Account.Id(1))[Closed]
  val selectorCreated = AggregateEventSelector(Transaction)(Transaction.Id(1))[Created]
  val selectorMyEvent = AggregateEventSelector(TestAggregate)(TestAggregate.Id(1))[TestAggregate.Event.MyEvent]

  val process = new Process(AccountProcessing)
  import process.ProcessAction._

  "AggregateEventSelector " should " have an instance for subtypes of events of first aggregate in list" in {
    type L = Account.type :: Transaction.type :: HNil
    "implicitly[ValidAggregateSelector[AggregateEventSelector[Account.type, Opened], L]]" should compile
    "implicitly[ValidAggregateSelector[AggregateEventSelector[Account.type, Closed], L]]" should compile
  }

  "AggregateEventSelector " should " have an instance for subtypes of events of second (and last) aggregate in list" in {
    type L = Account.type :: Transaction.type :: HNil
    "implicitly[ValidAggregateSelector[AggregateEventSelector[Transaction.type, Created], L]]" should compile
  }

  "AggregateEventSelector " should " have no instance for events of aggregate not list" in {
    type L = Account.type :: HNil
    "implicitly[ValidAggregateSelector[AggregateEventSelector[Transaction.type, Created], L]]" shouldNot compile
  }

  "Process " should " allow selector for aggregate in the same bounded context" in {
    "AwaitEvent(selectorOpened)" should compile
    "AwaitEvent(selectorClosed)" should compile
    "AwaitEvent(selectorCreated)" should compile
  }

  "Process " should " not allow selector for aggregate not in the same context" in {
    "AwaitEvent(selectorMyEvent)" shouldNot compile
  }
}

//TODO delete
object Experiments {
  val selectorOpened = AggregateEventSelector(Account)(Account.Id(1))[Opened]
  val selectorClosed = AggregateEventSelector(Account)(Account.Id(1))[Closed]

  def sel[S <: EventSelector.WithEventType](s: S)(implicit selector: EventSelector[S]) = selector
  val s = sel(selectorOpened)
  val b = s.castEvent(???)
  println(b.owner)
}
