package slfes.syntax

import shapeless.ops.coproduct.Inject
import shapeless.{ Coproduct, Poly1 }

trait EventApplicator[State, Events <: Coproduct] extends Poly1 {
  type IsEvent[Event] = Inject[Events, Event]
  def on[Event: IsEvent](f: Event ⇒ State ⇒ State): Case.Aux[Event, State ⇒ State] = at(e ⇒ s ⇒ f(e)(s))
}