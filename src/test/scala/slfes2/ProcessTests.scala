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

  "Process " should " have a nice syntax to wait for events from aggregates " in {
    val account1 = Account.Id(1)
    val r = on(account1).await[Opened]
    r shouldBe await(AggregateEventSelector(Account)(account1)[Opened])
  }

  "Process " should " not allow selector for aggregate not in the same context" in {
    "AwaitEvent(selectorMyEvent)" shouldNot compile
    "await(selectorMyEvent)" shouldNot compile
  }

  "Process " should " allow commands of aggregates in the same context" in {
    """Execute[Account.type](Account, Account.Id(1), Open("Mario"))""" should compile
    """on(Account.Id(1)).execute(Open("Mario"))""" should compile
  }

  "Process " should " not allow commands of aggregates not in the same context" in {
    """Execute[TestAggregate.type](TestAggregate, TestAggregate.Id(1), TestAggregate.Command.MyCommand("Mario"))""" shouldNot compile
    """on(TestAggregate.Id(1)).execute(TestAggregate.Command.MyCommand("Mario"))""" shouldNot compile
  }

  "Process " should " not allow commands that don't fit the aggregate" in {
    """on(Transaction.Id(1)).execute(Open("Mario"))""" shouldNot compile
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
    tx ← from(tid).await[Created]
    _ ← on(tx.from).execute(BlockFunds(Transaction.Id(1), tx.amount))
    _ ← from(tx.from).await[Blocked]
    _ ← on(tid).execute(Confirm())
  } yield ()

  //TODO error handling for commands (force it)
  //TODO how to access event metadata
  //TODO firstOf syntax

}
