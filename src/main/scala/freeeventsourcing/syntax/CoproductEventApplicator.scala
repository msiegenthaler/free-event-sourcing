package freeeventsourcing.syntax

import scala.annotation.implicitNotFound
import shapeless.ops.coproduct.Folder
import shapeless.{ Coproduct, Generic, Poly, Poly1 }
import freeeventsourcing.syntax.CoproductEventApplicator.ApplyEvent

trait CoproductEventApplicator[Event, State] extends Poly1 {
  def function[CE <: Coproduct](implicit
    generic: Generic.Aux[Event, CE],
    folder: ApplyEvent[this.type, CE, Event, State]): (Event, State) ⇒ State = (event, state) ⇒ {
    val cc = generic.to(event)
    cc.fold(this).apply(state)
  }

  def on[E <: Event](f: E ⇒ State ⇒ State): Case.Aux[E, State ⇒ State] = at(f)
}
object CoproductEventApplicator {
  @implicitNotFound("Not all events (subclasses of ${Event}) are handled in ${X}.")
  type ApplyEvent[X <: Poly, CE <: Coproduct, Event, State] = Folder.Aux[X, CE, State ⇒ State]
}