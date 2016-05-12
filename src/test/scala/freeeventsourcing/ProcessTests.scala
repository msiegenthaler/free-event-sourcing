package freeeventsourcing

import cats.data.Xor
import org.scalatest.{ FlatSpec, Matchers }
import shapeless.{ ::, CNil, HNil, :+: }
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
    val r = from(account1).await[Opened]
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

  "Process.switch " should " accept a single selector with an execute (flatMap)" in {
    val r = switch(_.
      on(selectorOpened).execute((e: Opened) ⇒ terminate))
    "r : ProcessMonad[Unit :+: CNil]" should compile

    val r2 = switch(_.
      on(selectorOpened).flatMap((e: Opened) ⇒ terminate))
    "r2 : ProcessMonad[Unit :+: CNil]" should compile
  }

  "Process.switch " should " accept a single selector with a map" in {
    val r = switch(_.
      on(selectorOpened).map(_.owner))
    "r : ProcessMonad[String :+: CNil]" should compile
  }

  "Process.switch " should " accept a single selector that returns the event" in {
    val r = switch(_.
      on(selectorOpened).event)
    "r : ProcessMonad[Opened :+: CNil]" should compile
  }

  "Process.switch " should " accept a single selector that terminates the process" in {
    val r = switch(_.
      on(selectorOpened).terminate)
    "r : ProcessMonad[Unit :+: CNil]" should compile
  }

  "Process.switch " should " accept two selectors" in {
    val r = switch(_.
      on(selectorOpened).event.
      on(selectorClosed).event)
    "r : ProcessMonad[Closed :+: Opened :+: CNil]" should compile
  }

  "Process.switch " should " accept a single event from an aggregate" in {
    val r = switch(_.
      from(Account.Id(1)).on[Opened].event)
    "r : ProcessMonad[Opened :+: CNil]" should compile
  }

  "Process.switch " should " accept a two event from different aggregates" in {
    val r = switch(_.
      from(Account.Id(1)).on[Opened].event.
      from(Transaction.Id(1)).on[Created].event)
    "r : ProcessMonad[Created :+: Opened :+: CNil]" should compile
  }

  "Process.switch " should " allow to mix selectors and events from aggregates" in {
    val r = switch(_.
      from(Account.Id(1)).on[Opened].terminate.
      on(selectorClosed).event.
      from(Transaction.Id(1)).on[Created].event.
      on(selectorOpened).event)
    "r : ProcessMonad[Opened :+: Created :+: Closed :+: Unit :+: CNil]" should compile
  }
}