package freeeventsourcing.syntax

import java.time.Instant
import cats.Monad
import freeeventsourcing._
import freeeventsourcing.accountprocessing.Account.Command._
import freeeventsourcing.accountprocessing.Account.Error._
import freeeventsourcing.accountprocessing.Account.Event._
import freeeventsourcing.accountprocessing.Transaction.Command._
import freeeventsourcing.accountprocessing.Transaction.Error._
import freeeventsourcing.accountprocessing.Transaction.Event._
import freeeventsourcing.accountprocessing.{ Account, AccountProcessing, Transaction }
import org.scalatest.{ FlatSpec, Matchers }
import shapeless.{ :+:, CNil, Coproduct }

class ProcessSyntaxTests extends FlatSpec with Matchers {
  val process = new ProcessSyntax(AccountProcessing)
  import process._

  val support = new ProcessTestSupport(AccountProcessing)
  import support._
  import ProcessTests._

  "ProcessSyntax.await " should " support event from selectors of the same bounded context" in {
    "await(selectorOpened)" should compile
    "await(selectorClosed)" should compile
    "await(selectorCreated)" should compile
  }

  "ProcessSyntax.await " should " not allow selector for aggregate not in the same context" in {
    "await(selectorMyEvent)" shouldNot compile
  }

  "ProcessSyntax.await " should " result in a Await action" in {
    await(selectorOpened) should runFromWithResult(
      Expect.awaitEvent(selectorOpened)(Opened("Mario"))
    )(Opened("Mario"))
  }

  "ProcessSyntax " should " have a nice syntax to wait for events from aggregates " in {
    val account1 = Account.Id(1)
    val r = from(account1).await[Opened]
    r shouldBe await(AggregateEventSelector(Account)(account1)[Opened])
  }

  "ProcessSyntax.on().execute " should " allow commands of aggregates in the same context" in {
    """on(Account.Id(1)).execute(Open("Mario"))(???)""" should compile
  }

  "ProcessSyntax.on().execute " should " execute the command and return unit if successful" in {
    val cmd = BlockFunds(Transaction.Id(2), 1)
    on(Account.Id(1)).execute(cmd)(
      _.catching[InsufficientFunds](_ ⇒ terminate).
        catching[NotOpen](_ ⇒ terminate)
    ) should runFromWithResult(
        Expect.commandSuccessful(Account, Account.Id(1), cmd)
      )(())
  }

  "ProcessSyntax.on().execute " should " execute the command and run the command handler if command failed" in {
    val cmd = BlockFunds(Transaction.Id(2), 1)
    on(Account.Id(1)).execute(cmd)(
      _.catching[InsufficientFunds](_ ⇒ terminate).
        catching[NotOpen](_ ⇒ terminate)
    ) should runFromWithResult(
        Expect.commandFailed(Account, Account.Id(1), cmd)(InsufficientFunds()),
        Expect.end
      )(())
  }

  "ProcessSyntax.on().execute " should " execute the command and run the command handler that does nothing" in {
    val cmd = BlockFunds(Transaction.Id(1), 2)
    on(Account.Id(0)).execute(cmd)(
      _.catching[InsufficientFunds](_ ⇒ noop).
        catching[NotOpen](_ ⇒ terminate)
    ) should runFromWithResult(
        Expect.commandFailed(Account, Account.Id(0), cmd)(InsufficientFunds())
      )(())
  }

  "ProcessSyntax.on().execute " should " not allow commands of aggregates not in the same context" in {
    """on(TestAggregate.Id(1)).execute(TestAggregate.Command.MyCommand("Mario"))(???)""" shouldNot compile
  }

  "ProcessSyntax.on().execute " should " not allow commands that don't fit the aggregate" in {
    """on(Transaction.Id(1)).execute(Open("Mario"))(???)""" shouldNot compile
  }

  "ProcessSyntax.on().execute " should " have a nice syntax to catch multiple errors on a command" in {
    on(Account.Id(1)).execute(BlockFunds(Transaction.Id(2), 1)) {
      _.catching[InsufficientFunds](_ ⇒ terminate).
        catching[NotOpen](_ ⇒ terminate)
    }
  }

  "ProcessSyntax.on().execute " should " require that executing a command contains handlers for all errors" in {
    """
    on(Account.Id(1)).execute(BlockFunds(Transaction.Id(1), 100)) {
      _.catching[InsufficientFunds](_ ⇒ terminate)
    }""" shouldNot compile
  }

  "ProcessSyntax.on().execute " should " ensure that only errors that are thrown by the command are handled" in {
    """
    on(Account.Id(1)).execute(BlockFunds(Transaction.Id(2), 100)) {
      _.catching[InsufficientFunds](_ ⇒ terminate).
        catching[AlreadyOpen](_ ⇒ terminate)
    }""" shouldNot compile
  }

  "ProcessSyntax.firstOf " should " accept a single selector with an execute (flatMap)" in {
    val r = firstOf(_.
      on(selectorOpened).execute((e: Opened) ⇒ terminate))
    "r : ProcessMonad[Unit :+: CNil]" should compile

    val r2 = firstOf(_.
      on(selectorOpened).flatMap((e: Opened) ⇒ terminate))
    "r2 : ProcessMonad[Unit :+: CNil]" should compile
  }

  "ProcessSyntax.firstOf " should " accept a single selector with a map" in {
    val r = firstOf(_.
      on(selectorOpened).map(_.owner))
    "r : ProcessMonad[String :+: CNil]" should compile
  }

  "ProcessSyntax.firstOf " should " accept a single selector that returns the event" in {
    val r = firstOf(_.
      on(selectorOpened).event)
    "r : ProcessMonad[Opened :+: CNil]" should compile
  }

  "ProcessSyntax.firstOf " should " accept a single selector that terminates the process" in {
    val r = firstOf(_.
      on(selectorOpened).terminate)
    "r : ProcessMonad[Unit :+: CNil]" should compile
  }

  "ProcessSyntax.firstOf " should " accept two selectors" in {
    val r = firstOf(_.
      on(selectorOpened).event.
      on(selectorClosed).event)
    "r : ProcessMonad[Opened :+: Closed:+: CNil]" should compile
  }

  "ProcessSyntax.firstOf " should " accept a single event from an aggregate" in {
    val r = firstOf(_.
      from(Account.Id(1)).on[Opened].event)
    "r : ProcessMonad[Opened :+: CNil]" should compile
  }

  "ProcessSyntax.firstOf " should " accept a two event from different aggregates" in {
    val r = firstOf(_.
      from(Account.Id(1)).on[Opened].event.
      from(Transaction.Id(1)).on[Created].event)
    "r : ProcessMonad[Opened :+: Created :+: CNil]" should compile
  }

  "ProcessSyntax.firstOf " should " allow to mix selectors and events from aggregates" in {
    val r = firstOf(_.
      from(Account.Id(1)).on[Opened].terminate.
      on(selectorClosed).event.
      from(Transaction.Id(1)).on[Created].event.
      on(selectorOpened).event)
    "r : ProcessMonad[Unit :+: Closed :+: Created :+: Opened :+: CNil]" should compile
  }

  "ProcessSyntax.firstOfUnified " should " accept a single selector and return the event type" in {
    val r = firstOfUnified(_.
      on(selectorOpened).event)
    "r : ProcessMonad[Opened]" should compile
  }

  "ProcessSyntax.firstOfUnified " should " accept a single selector with a map and return the result type of the map" in {
    val r = firstOfUnified(_.
      on(selectorOpened).map(_.owner))
    "r : ProcessMonad[String]" should compile
  }

  "ProcessSyntax.firstOfUnified " should " accept a two selector and return the base type of the event type" in {
    val r = firstOfUnified(_.
      on(selectorOpened).event.
      on(selectorClosed).event)
    "r : ProcessMonad[Account.Event with Product with Serializable]" should compile
  }

  "ProcessSyntax.firstOfUnified " should " accept a two selector mapping to the same type and return this type" in {
    val r = firstOfUnified(_.
      on(selectorOpened).map(_ ⇒ "hi").
      on(selectorClosed).map(_ ⇒ "there"))
    "r : ProcessMonad[String]" should compile
  }

  "ProcessSyntax.firstOf " should " accept a timeout" in {
    val r = firstOf(_.
      timeout(Instant.now)(terminate))
    "r : ProcessMonad[Unit :+: CNil]" should compile
  }

  "ProcessSyntax.firstOf " should " accept a timeout and a selector" in {
    val r = firstOf(_.
      on(selectorClosed).event.
      timeout(Instant.now)(terminate))
    "r : ProcessMonad[Closed :+: Unit :+: CNil]" should compile
  }

  "ProcessSyntax.firstOf " should " accept a timeout that returns a value and a selector" in {
    val r = firstOf(_.
      on(selectorClosed).event.
      timeout(Instant.now)(Monad[ProcessMonad].pure("timeout")))
    "r : ProcessMonad[Closed :+: String :+: CNil]" should compile
  }

  "ProcessSyntax.firstOf(_.selector.event) " should " should use one selectors and return the event" in {
    val evt = Opened("Mario")
    firstOf(_.
      on(selectorOpened).event) should runFromWithResult(
      Expect.firstOf(FirstOfSelector(selectorOpened))(0, evt)
    )(Coproduct[Opened :+: CNil](evt))
  }

  "ProcessSyntax.firstOf(_.waitUntil.noop) " should " should use one selectors and return unit" in {
    val instant = Instant.now()
    firstOf(_.
      timeout(instant)(noop)) should runFromWithResult(
      Expect.firstOf(FirstOfUntil(instant))(0, ())
    )(Coproduct[Unit :+: CNil](()))
  }

  "ProcessSyntax.firstOf(two _.selector.event).1 " should " should use two selectors and return the first event" in {
    val evt = Closed()
    firstOf(_.
      on(selectorOpened).event.
      on(selectorClosed).event) should runFromWithResult(
      Expect.firstOf(
        FirstOfSelector(selectorOpened),
        FirstOfSelector(selectorClosed)
      )(1, evt)
    )(Coproduct[Opened :+: Closed :+: CNil](evt))
  }

  "ProcessSyntax.firstOf(two _.selector.event).2 " should " should use two selectors and return the second event" in {
    val evt = Opened("Mario")
    firstOf(_.
      on(selectorOpened).event.
      on(selectorClosed).event) should runFromWithResult(
      Expect.firstOf(
        FirstOfSelector(selectorOpened),
        FirstOfSelector(selectorClosed)
      )(0, evt)
    )(Coproduct[Opened :+: Closed :+: CNil](evt))
  }

  "ProcessSyntax.firstOf(two _.selector.event and timeout).1 " should " should use two selectors, a timeout and return the first event" in {
    val evt = Opened("Mario")
    val instant = Instant.now()
    val p = firstOf(_.
      on(selectorOpened).event.
      on(selectorClosed).event.
      timeout(instant)(noop))

    val expect = Expect.firstOf(
      FirstOfSelector(selectorOpened),
      FirstOfSelector(selectorClosed),
      FirstOfUntil(instant)
    )(0, evt)

    p should runFromWithResult(
      expect
    )(Coproduct[Opened :+: Closed :+: Unit :+: CNil](evt))
  }

  "ProcessSyntax.firstOf(two _.selector.event and timeout).2 " should " should use two selectors, a timeout and return the second event" in {
    val evt = Closed()
    val instant = Instant.now()
    val p = firstOf(_.
      on(selectorOpened).event.
      on(selectorClosed).event.
      timeout(instant)(noop))

    val expect = Expect.firstOf(
      FirstOfSelector(selectorOpened),
      FirstOfSelector(selectorClosed),
      FirstOfUntil(instant)
    )(1, evt)

    p should runFromWithResult(
      expect
    )(Coproduct[Opened :+: Closed :+: Unit :+: CNil](evt))
  }

  "ProcessSyntax.firstOf(two _.selector.event with map and timeout).1 " should " should use two selectors, a timeout and return the mapped first event" in {
    val instant = Instant.now()
    val p = firstOf(_.
      on(selectorOpened).map(_.owner).
      on(selectorClosed).event.
      timeout(instant)(noop))

    val expect = Expect.firstOf(
      FirstOfSelector(selectorOpened),
      FirstOfSelector(selectorClosed),
      FirstOfUntil(instant)
    )(0, Opened("Mario"))

    p should runFromWithResult(
      expect
    )(Coproduct[String :+: Closed :+: Unit :+: CNil]("Mario"))
  }

  "ProcessSyntax.firstOf(two _.selector.event with flatMap to terminate).1 " should " should use two selectors, a timeout and return the unit and end" in {
    val instant = Instant.now()
    val p = firstOf(_.
      on(selectorOpened).flatMap(_ ⇒ terminate).
      on(selectorClosed).event.
      timeout(instant)(noop))

    val expect = Expect.firstOf(
      FirstOfSelector(selectorOpened),
      FirstOfSelector(selectorClosed),
      FirstOfUntil(instant)
    )(0, Opened("Mario"))

    p should runFromWithResult(
      expect,
      Expect.end
    )(Coproduct[Unit :+: Closed :+: Unit :+: CNil](()))
  }

  "ProcessSyntax.firstOf(two _.selector.event and timeout).3 " should " should use two selectors, a timeout and return unit" in {
    val instant = Instant.now()
    val p = firstOf(_.
      on(selectorOpened).event.
      on(selectorClosed).event.
      timeout(instant)(noop))

    val expect = Expect.firstOf(
      FirstOfSelector(selectorOpened),
      FirstOfSelector(selectorClosed),
      FirstOfUntil(instant)
    )(2, ())

    p should runFromWithResult(
      expect
    )(Coproduct[Opened :+: Closed :+: Unit :+: CNil](()))
  }

  "ProcessSyntax.firstOf(two _.selector.event and timeout(terminate)).3 " should " should use two selectors, a timeout and return unit and then end" in {
    val instant = Instant.now()
    val p = firstOf(_.
      on(selectorOpened).event.
      on(selectorClosed).event.
      timeout(instant)(terminate))

    val expect = Expect.firstOf(
      FirstOfSelector(selectorOpened),
      FirstOfSelector(selectorClosed),
      FirstOfUntil(instant)
    )(2, ())

    p should runFromWithResult(
      expect,
      Expect.end
    )(Coproduct[Opened :+: Closed :+: Unit :+: CNil](()))
  }

  "ProcessSyntax.firstOf(two _.selector.event and timeout(mapToString).3 " should " should use two selectors, a timeout and return a string" in {
    val instant = Instant.now()
    val p = firstOf(_.
      on(selectorOpened).event.
      on(selectorClosed).event.
      timeout(instant)(Monad[ProcessMonad].pure("hi there")))

    val expect = Expect.firstOf(
      FirstOfSelector(selectorOpened),
      FirstOfSelector(selectorClosed),
      FirstOfUntil(instant)
    )(2, ())

    p should runFromWithResult(
      expect
    )(Coproduct[Opened :+: Closed :+: String :+: CNil]("hi there"))
  }

  //TODO more tests

  "ProcessSyntax.value " should " do nothing and return the value" in {
    process.value(1) should runFromWithResult()(1)
    process.value("hi") should runFromWithResult()("hi")
  }

  "ProcessSyntax.noop " should " do nothing and return unit" in {
    noop should runFromWithResult()(())
  }

  "ProcessSyntax.terminate " should " end the process and return unit" in {
    terminate should runFromWithResult(Expect.end)(())
  }

  "ProcessSyntax.waitUntil " should " accept a specific time and return unit" in {
    val i = Instant.now
    waitUntil(i) should runFromWithResult(Expect.waitUntil(i))(())
  }
}
