package slfes2

import cats.data.Xor
import org.scalatest.{ FlatSpec, Matchers }
import shapeless.{ ::, CNil, HNil }
import slfes2.accountprocessing.Account.Command.{ BlockFunds, Open }
import slfes2.accountprocessing.Account.Error.{ AlreadyOpen, InsufficientFunds, NotOpen }
import slfes2.accountprocessing.{ Account, AccountProcessing, Transaction }
import slfes2.accountprocessing.Account.Event.{ Blocked, Closed, Opened }
import slfes2.accountprocessing.Transaction.Command.Confirm
import slfes2.accountprocessing.Transaction.Error.{ AlreadyCanceled, DoesNotExist }
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

  "Process.firstOf " should " reject an empty selector" in {
    "firstOf(identity)" shouldNot compile
  }

  "Process.firstOf " should " accept a single selector" in {
    val a: ProcessMonad[Opened] = firstOf(_.await(selectorOpened))
  }

  "Process.firstOf " should " accept two selectors" in {
    val a: ProcessMonad[Account.Event] = firstOf(_.
      await(selectorOpened).
      await(selectorClosed))
  }

  "Process.firstOf " should " accept three selectors" in {
    //TODO this is not really helpful.. what should we use as the return type? probably have another ProcessMonad as the parameter to each alternative..
    val a = firstOf(_.
      await(selectorOpened).
      await(selectorClosed).
      await(selectorCreated))
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

    //TODO convert to test
    _ ← firstOf(_.
      await(selectorBlocked))

    //TODO convert to test
    x ← firstOf(_.
      await(selectorOpened).
      await(selectorBlocked))

    //TODO convert to test
    x ← firstOf(_.
      from(Account.Id(1)).await[Opened])

    //TODO convert to test
    x ← firstOf(_.
      from(Account.Id(1)).await[Opened].
      from(Account.Id(2)).await[Closed])

    //TODO convert to test
    x ← firstOf(_.
      from(Account.Id(1)).await[Opened].
      from(Account.Id(2)).await[Closed].
      await(selectorBlocked))

    _ ← from(tx.from).await[Blocked]
    _ ← on(tid).execute(Confirm()) {
      _.catching[AlreadyCanceled](_ ⇒ terminate).
        catching[DoesNotExist](_ ⇒ terminate)
    }
  } yield x

  //TODO how to access event metadata
  //TODO all syntax?

}
