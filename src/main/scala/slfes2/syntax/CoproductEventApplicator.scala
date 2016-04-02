package slfes2.syntax

import scala.annotation.implicitNotFound
import shapeless.ops.coproduct.Folder
import shapeless.{ Coproduct, Generic, Poly, Poly1 }
import slfes2.syntax.CoproductEventApplicator.ApplyEvent

abstract class CoproductEventApplicator[Event, State] extends Poly1 {
  def apply[CE <: Coproduct](
    event: Event, state: State
  )(implicit
    generic: Generic.Aux[Event, CE],
    folder: ApplyEvent[this.type, CE, Event, State]): State = {
    val cc = generic.to(event)
    cc.fold(this).apply(state)
  }

}
object CoproductEventApplicator {
  @implicitNotFound("Not all events (subclasses of ${Event}) are handled in ${X}.")
  type ApplyEvent[X <: Poly, CE <: Coproduct, Event, State] = Folder.Aux[X, CE, State ⇒ State]
}
