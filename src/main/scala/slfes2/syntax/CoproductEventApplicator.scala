package slfes2.syntax

import shapeless.ops.coproduct.Folder
import shapeless.{ Coproduct, Generic, Poly1 }

import scala.annotation.implicitNotFound

abstract class CoproductEventApplicator[Event, State] extends Poly1 {
  def apply[CE <: Coproduct](
    event: Event, state: State
  )(implicit
    generic: Generic.Aux[Event, CE],
    folder: ApplyEvent[CE]): State = {
    val cc = generic.to(event)
    cc.fold(this).apply(state)
  }

  @implicitNotFound("Not all events for the aggregate are handled.")
  type ApplyEvent[CE <: Coproduct] = Folder.Aux[this.type, CE, State ⇒ State]
}
