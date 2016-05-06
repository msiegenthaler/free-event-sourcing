package slfes2

import org.scalatest.{ FlatSpec, Matchers }
import shapeless.{ ::, CNil, HNil }
import slfes2.accountprocessing.Account.Command.{ BlockFunds, Open }
import slfes2.accountprocessing.{ Account, AccountProcessing, Transaction }
import slfes2.accountprocessing.Account.Event.{ Blocked, Closed, Opened }
import slfes2.accountprocessing.Transaction.Command.Confirm
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
    object Command {
      case class MyCommand(value: String) extends Command {
        type Error = CNil
      }
    }
  }

  val selectorOpened = AggregateEventSelector(Account)(Account.Id(1))[Opened]
  val selectorClosed = AggregateEventSelector(Account)(Account.Id(1))[Closed]
  val selectorCreated = AggregateEventSelector(Transaction)(Transaction.Id(1))[Created]
  val selectorMyEvent = AggregateEventSelector(TestAggregate)(TestAggregate.Id(1))[TestAggregate.Event.MyEvent]

  val process = new Process(AccountProcessing)
  import process.ProcessAction._
  import process.Syntax
  import process.Syntax._

  "Process " should " allow selector for aggregate in the same bounded context" in {
    "AwaitEvent(selectorOpened)" should compile
    "AwaitEvent(selectorClosed)" should compile
    "AwaitEvent(selectorCreated)" should compile

    "await(selectorOpened)" should compile
    "await(selectorClosed)" should compile
    "await(selectorCreated)" should compile
  }

  "Process " should " not allow selector for aggregate not in the same context" in {
    "AwaitEvent(selectorMyEvent)" shouldNot compile
    "await(selectorMyEvent)" shouldNot compile
  }

  "Process " should " allow commands of aggregates in the same context" in {
    """Execute[Account.type](Account, Account.Id(1), Open("Mario"))""" should compile
    """Syntax.execute(Account)(Account.Id(1), Open("Mario"))""" should compile
  }

  "Process " should " not allow commands of aggregates not in the same context" in {
    """Execute[TestAggregate.type](TestAggregate, TestAggregate.Id(1), TestAggregate.Command.MyCommand("Mario"))""" shouldNot compile
    """Syntax.execute(TestAggregate)(TestAggregate.Id(1), TestAggregate.Command.MyCommand("Mario"))""" shouldNot compile
  }
}

//TODO delete
object Experiments {
  val selectorOpened = AggregateEventSelector(Account)(Account.Id(1))[Opened]
  def sel[S <: EventSelector.WithEventType](s: S)(implicit selector: EventSelector[S]) = selector
  val b = sel(selectorOpened).castEvent(???)
  println(b.owner)

  val process = new Process(AccountProcessing)
  import process.Syntax
  import process.Syntax._

  val tid = Transaction.Id(1)
  for {
    tx ← awaitFrom(Transaction)(tid)[Created]
    _ ← Syntax.execute(Account)(tx.from, BlockFunds(Transaction.Id(1), tx.amount))
    _ ← awaitFrom(Account)(tx.from)[Blocked]
    _ ← Syntax.execute(Transaction)(tid, Confirm())
  } yield ()
  //TODO error handling for commands (force it)
  //TODO how to access event metadata
  //TODO firstOf syntax
  //TODO resolve the aggregate from the id type (get rid of the first parameter block in execute and await from)

}
