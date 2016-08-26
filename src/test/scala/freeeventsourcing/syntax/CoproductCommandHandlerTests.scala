package freeeventsourcing.syntax

import cats.data.Xor
import freeeventsourcing.api.AggregateCommand
import org.scalatest.{ FlatSpec, Matchers }
import shapeless.{ :+:, CNil, Coproduct }

class CoproductCommandHandlerTests extends FlatSpec with Matchers {

  sealed trait Event
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
      def handle[C <: Command](command: C, state: State) = doHandle(command).apply(state)

      implicit val first = commandCase[FirstCommand] { cmd ⇒ s: String ⇒
        Xor.right(Event1(s) :: Nil)
      }
      implicit val second = commandCase[SecondCommand] { cmd ⇒ s: String ⇒
        Xor.right(Nil)
      }
    }
    H(FirstCommand("mario"), "state") shouldBe Xor.right(List(Event1("state")))
    H(SecondCommand(1), "state") shouldBe Xor.right(Nil)
  }

  "CoproductCommandHandler " should " allow handling commands with errors" in {
    object H extends CoproductCommandHandler[Command, Event, String] {
      def handle[C <: Command](command: C, state: State) = doHandle(command).apply(state)

      implicit val first = commandCase[FirstCommand] { cmd ⇒ s: String ⇒
        val e = Coproduct[FirstCommand#Error](ErrorTwo(s))
        Xor.left(e)
      }
      implicit val second = commandCase[SecondCommand] { cmd ⇒ s: String ⇒
        Xor.right(Nil)
      }
    }
    H(FirstCommand("mario"), "state") shouldBe Xor.left(Coproduct[FirstCommand#Error](ErrorTwo("state")))
    H(SecondCommand(1), "state") shouldBe Xor.right(Nil)
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
      |      def handle[C <: Command](command: C, state: State) = doHandle(command).apply(state)
      |      implicit val first = commandCase[FirstCommand] { cmd ⇒
      |        s: String ⇒
      |          Xor.right(Event1() :: Nil)
      |      }
      |    }
    """.stripMargin shouldNot compile

    """
      |    object H extends CoproductCommandHandler[Command, String, Event] {
      |      def handle[C <: Command](command: C, state: State) = doHandle(command).apply(state)
      |      implicit val second = commandCase[SecondCommand] { cmd ⇒
      |        s: String ⇒
      |          Xor.right(Seq.empty)
      |      }
      |    }
    """.stripMargin shouldNot compile
  }
}
