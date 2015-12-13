import cats.free.Free

package object fes {
  object CommandEffect {
    sealed trait CommandOp[+Event, +State, +A]
    case class Current[State]() extends CommandOp[Nothing, State, State]
    case class Emit[Event](event: Event) extends CommandOp[Event, Nothing, Unit]
    case class Fail(reason: String) extends CommandOp[Nothing, Nothing, Nothing]
  }

  /** Monad that captures the outcome of command processing. */
  type CommandEffect[Event, State, A] = Free[CommandEffect.CommandOp[Event, State, ?], A]
}
