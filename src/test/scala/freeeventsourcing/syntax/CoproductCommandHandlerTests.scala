package freeeventsourcing.syntax

import cats.data.Xor
import freeeventsourcing.AggregateCommand
import org.scalatest.{ FlatSpec, Matchers }
import shapeless.CNil

class CoproductCommandHandlerTests extends FlatSpec with Matchers {

  sealed trait Event
  object Event {
    case class Event1() extends Event
  }

  sealed trait Command extends AggregateCommand
  object Command {
    case class FirstCommand(arg: String) extends Command {
      type Error = CNil
    }
    case class SecondCommand(arg: Int) extends Command {
      type Error = CNil
    }
  }
  import Command._
  import Event._

  "CoproductCommandHandler " should " allow handling commands" in {
    object H extends CoproductCommandHandler[Command, String, Event] {
      def handle[C <: Command](command: C, state: State) = doHandle(command).apply(state)
      implicit val first = at[FirstCommand] { cmd ⇒ s: String ⇒
        Xor.right(Event1() :: Nil)
      }
      implicit val second = at[SecondCommand] { cmd ⇒ s: String ⇒
        Xor.right(Nil)
      }
    }
  }

  "CoproductCommandHandler " should " fail to compile if no commands are handled" in {
    """
      | object H extends CoproductCommandHandler[Command, String, Event] {
      |   def handle[C <: Command](command: C, state: State) = doHandle(command).apply(state)
      | }
    """.stripMargin shouldNot compile
  }

  "CoproductCommandHandler " should " fail to compile if not all commands are handled" in {
    """
      | object H extends CoproductCommandHandler[Command, String, Event] {
      |   def handle[C <: Command](command: C, state: State) = doHandle(command).apply(state)
      |   implicit val first = at[FirstCommand] { cmd ⇒ s: String ⇒
      |     Xor.right(Event1() :: Nil)
      |   }
      | }
    """.stripMargin shouldNot compile

    """
      | object H extends CoproductCommandHandler[Command, String, Event] {
      |   def handle[C <: Command](command: C, state: State) = doHandle(command).apply(state)
      |   implicit val second = at[SecondCommand] { cmd ⇒ s: String ⇒
      |     Xor.right(Seq.empty)
      |   }
      | }
    """.stripMargin shouldNot compile
  }
}
