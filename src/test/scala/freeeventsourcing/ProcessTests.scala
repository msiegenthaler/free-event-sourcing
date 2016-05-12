package freeeventsourcing

import cats.data.Xor
import org.scalatest.{ FlatSpec, Matchers }
import shapeless.{ ::, CNil, HNil }
import freeeventsourcing.accountprocessing.Account.Command.{ BlockFunds, Open }
import freeeventsourcing.accountprocessing.Account.Error.{ AlreadyOpen, InsufficientFunds, NotOpen }
import freeeventsourcing.accountprocessing.{ Account, AccountProcessing, Transaction }
import freeeventsourcing.accountprocessing.Account.Event.{ Blocked, Closed, Opened }
import freeeventsourcing.accountprocessing.Transaction.Command.Confirm
import freeeventsourcing.accountprocessing.Transaction.Error.{ AlreadyCanceled, DoesNotExist }
import freeeventsourcing.accountprocessing.Transaction.Event.Created

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
  import process.ProcessMonad
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
    """Execute[Account.type, Open](Account, Account.Id(1), Open("Mario"), ???)""" should compile
    """on(Account.Id(1)).execute(Open("Mario"))(_.catching[AlreadyOpen](_ ⇒ terminate))""" should compile
  }

  "Process " should " not allow commands of aggregates not in the same context" in {
    """Execute[TestAggregate.type, TestAggregate.Command.MyCommand](TestAggregate, TestAggregate.Id(1), TestAggregate.Command.MyCommand("Mario"), ???)""" shouldNot compile
    """on(TestAggregate.Id(1)).execute(TestAggregate.Command.MyCommand("Mario"))(???)""" shouldNot compile
  }

  "Process " should " not allow commands that don't fit the aggregate" in {
    """on(Transaction.Id(1)).execute(Open("Mario"))(???)""" shouldNot compile
  }

  "Process " should " have a nice syntax to catch multiple errors on a command" in {
    on(Account.Id(1)).execute(BlockFunds(Transaction.Id(2), 100)) {
      _.catching[InsufficientFunds](_ ⇒ terminate).
        catching[NotOpen](_ ⇒ terminate)
    }
  }

  "Process " should " require that executing a command contains handlers for all errors" in {
    """
    on(Account.Id(1)).execute(BlockFunds(Transaction.Id(1), 100)) {
      _.catching[InsufficientFunds](_ ⇒ terminate)
    }""" shouldNot compile
  }

  "Process " should " ensure that only errors that are thrown by the command are handled" in {
    """
    on(Account.Id(1)).execute(BlockFunds(Transaction.Id(2), 100)) {
      _.catching[InsufficientFunds](_ ⇒ terminate).
        catching[AlreadyOpen](_ ⇒ terminate)
    }""" shouldNot compile
  }

  "Process " should " allow the implementation to handle errors by the command" in {
    """
    def execHandler(exec: Execute[Account.type, Open], r: Open#Error Xor Unit): process.ProcessMonad[Unit] = {
      r.fold(exec.errorHandler.apply, _ ⇒ noop)
    }""" should compile
  }

  "Process.awaitFirstUnified " should " reject an empty selector" in {
    "awaitFirstUnified(identity)" shouldNot compile
  }

  "Process.awaitFirstUnified " should " accept a single selector and return the event type of the selector" in {
    val a: ProcessMonad[Opened] = awaitFirstUnified(_.event(selectorOpened))
  }

  "Process.awaitFirstUnified " should " accept an event from an aggregate and return the event type" in {
    val a: ProcessMonad[Opened] = awaitFirstUnified(_.from(Account.Id(1)).event[Opened])
  }

  "Process.awaitFirstUnified " should " accept two selectors from the same aggregate and unify that to the aggregates event type" in {
    val a: ProcessMonad[Account.Event with Product with Serializable] = awaitFirstUnified(_.
      event(selectorOpened).
      event(selectorClosed))
  }

  "Process.awaitFirstUnified " should " accept two events from the same aggregate and unify that to the aggregates event type" in {
    val a: ProcessMonad[Account.Event with Product with Serializable] = awaitFirstUnified(_.
      from(Account.Id(1)).event[Opened].
      from(Account.Id(1)).event[Closed])
  }

  "Process.awaitFirstUnified " should " accept three selectors with no common class and unify that to Product with Serializable" in {
    val a: Product with Serializable = awaitFirstUnified(_.
      event(selectorOpened).
      event(selectorClosed).
      event(selectorCreated))
  }

  "Process.awaitFirstUnified " should " accept events from different aggregates and unify that to Product with Serializable" in {
    val a: Product with Serializable = awaitFirstUnified(_.
      from(Account.Id(1)).event[Opened].
      from(Account.Id(1)).event[Closed].
      from(Transaction.Id(1)).event[Created])
  }
}

//TODO delete
object Experiments {
  val selectorOpened = AggregateEventSelector(Account)(Account.Id(1))[Opened]
  val selectorBlocked = AggregateEventSelector(Account)(Account.Id(1))[Blocked]
  def sel[S <: EventSelector.WithEventType](s: S)(implicit selector: EventSelector[S]) = selector
  val b = sel(selectorOpened).castEvent(???)
  println(b.owner)

  val process = new Process(AccountProcessing)
  import process.Syntax._

  val tid = Transaction.Id(1)
  val x = for {
    tx ← from(tid).await[Created]
    _ ← on(tx.from)
      .execute(BlockFunds(tid, tx.amount)) {
        _.catching[InsufficientFunds](_ ⇒ terminate).
          catching[NotOpen](_ ⇒ terminate)
      }

    _ ← switch(_.
      on(selectorOpened)(_ ⇒ noop).
      on(selectorBlocked)(t ⇒
        on(t.by).execute(Confirm()) {
          _.catching[AlreadyCanceled](_ ⇒ terminate).
            catching[DoesNotExist](_ ⇒ terminate)
        }))

    _ ← from(tx.from).await[Blocked]
    _ ← on(tid).execute(Confirm()) {
      _.catching[AlreadyCanceled](_ ⇒ terminate).
        catching[DoesNotExist](_ ⇒ terminate)
    }
  } yield ()

  //TODO how to access event metadata
  //TODO all syntax?

}
