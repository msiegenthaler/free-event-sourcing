import cats.data.Xor
import cats.free.Free
import shapeless.Coproduct

package object slfes {
  /** Super trait for all commands. */
  trait Cmd {
    type Errors <: Coproduct
  }

  type CmdResult[Events] = Xor[Any, Seq[Events]]

  /** Type for invariants. */
  type Inv[State] = (State ⇒ Boolean)

  type ProcessBody = ProcessBodyM[Unit]
  type ProcessBodyM[A] = Free[ProcessBodyAction, A]
}