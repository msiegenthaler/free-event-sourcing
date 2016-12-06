package freeeventsourcing.api.domainmodel

import freeeventsourcing.accountprocessing.Account.Event.{ Closed, Opened }
import freeeventsourcing.accountprocessing.Transaction.Event.Created
import freeeventsourcing.accountprocessing.{ Account, AccountProcessing, Transaction }
import freeeventsourcing.api.domainmodel.EventSelector.WithEventType
import freeeventsourcing.api.domainmodel.ProcessAction._
import freeeventsourcing.api.domainmodel.ProcessDefinition.ProcessMonad
import freeeventsourcing.api.domainmodel.eventselector.{ AggregateEventSelector, ValidSelector }
import org.scalatest.{ FlatSpec, Matchers }
import shapeless.CNil

class ProcessActionTests extends FlatSpec with Matchers {
  import ProcessActionTests._

  type AP = AccountProcessing.type

  def awaitEvent[S <: WithEventType: EventSelector](selector: S)(implicit ev: ValidSelector[AP, S]) =
    AwaitEvent[AP, S](selector)

  def executeAction[A <: Aggregate, Cmd <: A#Command](
    aggregateType: A, aggregate: A#Id, command: A#Command, errorHandler: Cmd#Error ⇒ ProcessMonad[AP, Unit]
  )(
    implicit
    ev: ValidAggregate[AP, A]
  ) = {
    Execute[AP, A, Cmd](aggregateType, aggregate, command, errorHandler)
  }

  "ProcessAction " should " allow selector for aggregate in the same bounded context" in {
    "awaitEvent(selectorOpened)" should compile
    "awaitEvent(selectorClosed)" should compile
    "awaitEvent(selectorCreated)" should compile
  }

  "ProcessAction " should " not allow selector for aggregate not in the same context" in {
    "awaitEvent(selectorMyEvent)" shouldNot compile
  }

  "ProcessAction " should " allow commands of aggregates in the same context" in {
    """
      import freeeventsourcing.accountprocessing.Account.Command.Open
      executeAction[Account.type, Open](Account, Account.Id(1), Open("Mario"), ???)""" should compile
  }

  "ProcessAction " should " not allow commands of aggregates not in the same context" in {
    """executeAction[TestAggregate.type, TestAggregate.Command.MyCommand](
        TestAggregate, TestAggregate.Id(1), TestAggregate.Command.MyCommand("Mario"), ???)""" shouldNot compile
  }

  "ProcessAction " should " allow the implementation to handle errors by the command" in {
    """
      import freeeventsourcing.accountprocessing.Account.Command.Open
      def execHandler(exec: Execute[AccountProcessing.type, Account.type, Open], r: Either[Open#Error, Unit]): ProcessMonad[AP, Unit] = {
        r.fold(exec.errorHandler.apply, _ ⇒ ???)
      }""" should compile
  }
}
object ProcessActionTests {
  object TestAggregate extends Aggregate {
    val name = "Test Aggregate"
    case class Id(id: Int)
    sealed trait Event extends DomainEvent
    object Event {
      final case class MyEvent(value: String) extends Event
    }
    sealed trait Command extends AggregateCommand
    object Command {
      final case class MyCommand(value: String) extends Command {
        type Error = CNil
      }
    }
  }
  case class OtherEvent() extends TestAggregate.Event

  val selectorOpened = AggregateEventSelector(Account)(Account.Id(1))[Opened]
  val selectorClosed = AggregateEventSelector(Account)(Account.Id(1))[Closed]
  val selectorCreated = AggregateEventSelector(Transaction)(Transaction.Id(1))[Created]
  val selectorMyEvent = AggregateEventSelector(TestAggregate)(TestAggregate.Id(1))[TestAggregate.Event.MyEvent]
}