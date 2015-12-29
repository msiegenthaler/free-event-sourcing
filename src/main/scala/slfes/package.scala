import shapeless.Coproduct

package object slfes {
  /** Super trait for all commands. */
  trait Cmd {
    type Errors <: Coproduct
  }
}
