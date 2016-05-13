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
    //    "AwaitEvent(selectorMyEvent)" shouldNot compile
    "await(selectorMyEvent)" shouldNot compile
  }

  "Process " should " allow commands of aggregates in the same context" in {
    //    """Execute[Account.type, Open](Account, Account.Id(1), Open("Mario"), ???)""" should compile
    """on(Account.Id(1)).execute(Open("Mario"))(_.catching[AlreadyOpen](_ ⇒ terminate))""" should compile
  }

  "Process " should " not allow commands of aggregates not in the same context" in {
    //    """Execute[TestAggregate.type, TestAggregate.Command.MyCommand](TestAggregate, TestAggregate.Id(1), TestAggregate.Command.MyCommand("Mario"), ???)""" shouldNot compile
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
    import cats.data.Xor
    import freeeventsourcing.ProcessAction.Execute
    def execHandler(exec: Execute[AccountProcessing.type, Account.type, Open], r: Open#Error Xor Unit): ProcessMonad[Unit] = {
      r.fold(exec.errorHandler.apply, _ ⇒ noop)
    }""" should compile
  }

  "Process.firstOf " should " accept a single selector with an execute (flatMap)" in {
    val r = firstOf(_.
      on(selectorOpened).execute((e: Opened) ⇒ terminate))
    "r : ProcessMonad[Unit :+: CNil]" should compile

    val r2 = firstOf(_.
      on(selectorOpened).flatMap((e: Opened) ⇒ terminate))
    "r2 : ProcessMonad[Unit :+: CNil]" should compile
  }

  "Process.firstOf " should " accept a single selector with a map" in {
    val r = firstOf(_.
      on(selectorOpened).map(_.owner))
    "r : ProcessMonad[String :+: CNil]" should compile
  }

  "Process.firstOf " should " accept a single selector that returns the event" in {
    val r = firstOf(_.
      on(selectorOpened).event)
    "r : ProcessMonad[Opened :+: CNil]" should compile
  }

  "Process.firstOf " should " accept a single selector that terminates the process" in {
    val r = firstOf(_.
      on(selectorOpened).terminate)
    "r : ProcessMonad[Unit :+: CNil]" should compile
  }

  "Process.firstOf " should " accept two selectors" in {
    val r = firstOf(_.
      on(selectorOpened).event.
      on(selectorClosed).event)
    "r : ProcessMonad[Closed :+: Opened :+: CNil]" should compile
  }

  "Process.firstOf " should " accept a single event from an aggregate" in {
    val r = firstOf(_.
      from(Account.Id(1)).on[Opened].event)
    "r : ProcessMonad[Opened :+: CNil]" should compile
  }

  "Process.firstOf " should " accept a two event from different aggregates" in {
    val r = firstOf(_.
      from(Account.Id(1)).on[Opened].event.
      from(Transaction.Id(1)).on[Created].event)
    "r : ProcessMonad[Created :+: Opened :+: CNil]" should compile
  }

  "Process.firstOf " should " allow to mix selectors and events from aggregates" in {
    val r = firstOf(_.
      from(Account.Id(1)).on[Opened].terminate.
      on(selectorClosed).event.
      from(Transaction.Id(1)).on[Created].event.
      on(selectorOpened).event)
    "r : ProcessMonad[Opened :+: Created :+: Closed :+: Unit :+: CNil]" should compile
  }

  "Process.firstOfUnified " should " accept a single selector and return the event type" in {
    val r = firstOfUnified(_.
      on(selectorOpened).event)
    "r : ProcessMonad[Opened]" should compile
  }

  "Process.firstOfUnified " should " accept a single selector with a map and return the result type of the map" in {
    val r = firstOfUnified(_.
      on(selectorOpened).map(_.owner))
    "r : ProcessMonad[String]" should compile
  }

  "Process.firstOfUnified " should " accept a two selector and return the base type of the event type" in {
    val r = firstOfUnified(_.
      on(selectorOpened).event.
      on(selectorClosed).event)
    "r : ProcessMonad[Account.Event with Product with Serializable]" should compile
  }

  "Process.firstOfUnified " should " accept a two selector mapping to the same type and return this type" in {
    val r = firstOfUnified(_.
      on(selectorOpened).map(_ ⇒ "hi").
      on(selectorClosed).map(_ ⇒ "there"))
    "r : ProcessMonad[String]" should compile
  }

  "Process.firstOf " should " accept a timeout" in {
    val r = firstOf(_.
      timeout(Instant.now)(terminate))
    "r : ProcessMonad[Unit :+: CNil]" should compile
  }

  "Process.firstOf " should " accept a timeout and a selector" in {
    val r = firstOf(_.
      on(selectorClosed).event.
      timeout(Instant.now)(terminate))
    "r : ProcessMonad[Unit :+: Closed :+: CNil]" should compile
  }

  "Process.firstOf " should " accept a timeout that returns a value and a selector" in {
    val r = firstOf(_.
      on(selectorClosed).event.
      timeout(Instant.now)(Monad[ProcessMonad].pure("timeout")))
    "r : ProcessMonad[String :+: Closed :+: CNil]" should compile
  }
}