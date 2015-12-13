import cats.free.Free

package object fes {
  object CommandEffect {
    sealed trait CommandOp[+Event, +State, +Error, +A]
    case class Current[State]() extends CommandOp[Nothing, State, Nothing, State]
    case class Emit[Event](event: Event) extends CommandOp[Event, Nothing, Nothing, Unit]
    case class Fail[Error](error: Error) extends CommandOp[Nothing, Nothing, Error, Nothing]
  }

  /** Monad that captures the outcome of command processing. */
  type CommandEffect[Event, State, Error, A] = Free[CommandEffect.CommandOp[Event, State, Error, ?], A]
}
