package freeeventsourcing.syntax

import freeeventsourcing.api.domainmodel.{ AggregateCommand, DomainEvent }
import org.scalatest.{ FlatSpec, Matchers }
import cats.instances.either._
import shapeless.{ :+:, CNil, Coproduct }

class CoproductCommandHandlerTests extends FlatSpec with Matchers {

  sealed trait Event extends DomainEvent
  object Event {
    case class Event1(a: String) extends Event
  }

  case class ErrorOne()
  case class ErrorTwo(a: String)

  sealed trait Command extends AggregateCommand
  object Command {
    case class FirstCommand(arg: String) extends Command {
      type Error = ErrorOne :+: ErrorTwo :+: CNil
    }
    case class SecondCommand(arg: Int) extends Command {
      type Error = ErrorOne :+: CNil
    }
  }
  import Command._
  import Event._

  "CoproductCommandHandler " should " allow handling commands" in {
    object H extends CoproductCommandHandler[Command, Event, String] {
      implicit val first = commandCase[FirstCommand] { cmd ⇒ s: String ⇒
        Right(Event1(s) :: Nil)
      }
      implicit val second = commandCase[SecondCommand] { cmd ⇒ s: String ⇒
        Right(Nil)
      }

      def handle[C <: Command](command: C, state: State) = doHandle(command).apply(state)
    }
    H(FirstCommand("mario"), "state") shouldBe Right(List(Event1("state")))
    H(SecondCommand(1), "state") shouldBe Right(Nil)
  }

  "CoproductCommandHandler " should " allow handling commands with errors" in {
    object H extends CoproductCommandHandler[Command, Event, String] {
      implicit val first = commandCase[FirstCommand] { cmd ⇒ s: String ⇒
        val e = Coproduct[FirstCommand#Error](ErrorTwo(s))
        Left(e)
      }
      implicit val second = commandCase[SecondCommand] { cmd ⇒ s: String ⇒
        Right(Nil)
      }

      def handle[C <: Command](command: C, state: State) = doHandle(command).apply(state)
    }
    H(FirstCommand("mario"), "state") shouldBe Left(Coproduct[FirstCommand#Error](ErrorTwo("state")))
    H(SecondCommand(1), "state") shouldBe Right(Nil)
  }

  "CoproductCommandHandler " should " fail to compile if no commands are handled" in {
    """
      | object H extends CoproductCommandHandler[Command, Event, String] {
      |   def handle[C <: Command](command: C, state: State) = doHandle(command).apply(state)
      | }
    """.stripMargin shouldNot compile
  }

  "CoproductCommandHandler " should " fail to compile if not all commands are handled" in {
    """
      |    object H extends CoproductCommandHandler[Command, Event, String] {
      |      implicit val first = commandCase[FirstCommand] { cmd ⇒
      |        s: String ⇒
      |          Right(Event1() :: Nil)
      |      }
      |      def handle[C <: Command](command: C, state: State) = doHandle(command).apply(state)
      |    }
    """.stripMargin shouldNot compile

    """
      |    object H extends CoproductCommandHandler[Command, String, Event] {
      |      implicit val second = commandCase[SecondCommand] { cmd ⇒
      |        s: String ⇒
      |          Right(Seq.empty)
      |      }
      |      def handle[C <: Command](command: C, state: State) = doHandle(command).apply(state)
      |    }
    """.stripMargin shouldNot compile
  }
}
