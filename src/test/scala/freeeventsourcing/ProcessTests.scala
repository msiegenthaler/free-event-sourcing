package freeeventsourcing

import java.time.Instant
import cats.Monad
import freeeventsourcing.accountprocessing.Account.Command.{ BlockFunds, Open }
import freeeventsourcing.accountprocessing.Account.Error.{ InsufficientFunds, NotOpen, AlreadyOpen }
import freeeventsourcing.accountprocessing.Account.Event.{ Closed, Opened }
import freeeventsourcing.accountprocessing.Transaction.Event.Created
import freeeventsourcing.accountprocessing.{ Account, AccountProcessing, Transaction }
import freeeventsourcing.syntax.ProcessSyntax
import org.scalatest.{ FlatSpec, Matchers }
import shapeless.{ CNil, :+: }

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

  val process = new ProcessSyntax(AccountProcessing)
  import process._

  "Process " should " allow selector for aggregate in the same bounded context" in {
    //    "AwaitEvent[AccountProcessing.type, selectorOpened.type](selectorOpened)" should compile
    //    "AwaitEvent[AccountProcessing.type, selectorClosed.type](selectorClosed)" should compile
    //    "AwaitEvent[AccountProcessing.type, selectorCreated.type](selectorCreated)" should compile
  }

  "Process " should " not allow selector for aggregate not in the same context" in {
    //    "AwaitEvent(selectorMyEvent)" shouldNot compile
  }

  "Process " should " allow commands of aggregates in the same context" in {
    //    """Execute[Account.type, Open](Account, Account.Id(1), Open("Mario"), ???)""" should compile
  }

  "Process " should " not allow commands of aggregates not in the same context" in {
    //    """Execute[TestAggregate.type, TestAggregate.Command.MyCommand](TestAggregate, TestAggregate.Id(1), TestAggregate.Command.MyCommand("Mario"), ???)""" shouldNot compile
  }

  "Process " should " allow the implementation to handle errors by the command" in {
    """
    import cats.data.Xor
    import freeeventsourcing.ProcessAction.Execute
    def execHandler(exec: Execute[AccountProcessing.type, Account.type, Open], r: Open#Error Xor Unit): ProcessMonad[Unit] = {
      r.fold(exec.errorHandler.apply, _ ⇒ noop)
    }""" should compile
  }
}