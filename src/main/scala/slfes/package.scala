import cats.data.Xor
import shapeless.Coproduct

package object slfes {
  /** Super trait for all commands. */
  trait Cmd {
    type Errors <: Coproduct
  }

  type CmdResult[Events] = Xor[Any, Seq[Events]]

  /** Type for invariants. */
  type Inv[State] = (State ⇒ Boolean)
}
