import cats.data.Xor
import cats.free.Free
import shapeless.ops.coproduct.Inject

package object slfes {
  /** Super trait for all commands. */
  type CmdResult[Events] = Xor[Any, Seq[Events]]

  /** Type for invariants. */
  type Inv[State] = (State ⇒ Boolean)

  type ProcessBody = ProcessBodyM[Unit]
  type ProcessBodyM[A] = Free[ProcessBodyAction, A]

  type AggregateEvt[A <: AggregateInterface] = Evt[A#Event, A]

  type CommandFor[A <: AggregateInterface] = {type λ[C] = Inject[A#Command, C]}
}